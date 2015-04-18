{-# LANGUAGE OverloadedStrings #-}

module Handler.CreatePaypalMembers where

import Import
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import Data.Maybe
import qualified Data.Text as T
import Data.Fixed
import Data.Time.Clock
import Util
import Control.Monad
import Data.Maybe

{-

  show (name, email) pairs
    that are in paypal transactions
      that are TO an email that belongs to the club
    that are NOT in account_email already
    show only one of each

  does account need to belong to a club as well?  
    I think yes.  
      you could derive the origin of money in the account by looking at the paypal
      transaction, but that's a pain.  want to do it with just the paypalinternal. 
    This expresses the idea that money is given to a club.  
    One can only have transactions from account to account within a club.  

-}

data UserMake = UserMake
  {
    paypals :: [PaypalId]
  }

userMakeForm :: [(Text,PaypalId)] -> Maybe UserMake -> Form UserMake
userMakeForm userlist mbum = renderDivs $ UserMake 
  <$> areq (checkboxesFieldList userlist) "valid users?" (paypals <$> mbum)

makeChex :: ClubId -> Handler ([(Text, PaypalId)], [PaypalId])
makeChex cid = do  
  membs <- runDB $ E.select $ E.from $ 
    (\(E.InnerJoin paypal email) -> do
      E.on $ (paypal ^. PaypalFromemail E.==. email E.?. EmailId)
        E.&&. (E.notExists $ 
                E.from $ \accountemail -> do
                  E.where_ (paypal ^. PaypalFromemail E.==. 
                            accountemail E.?. AccountEmailEmail))
        E.&&. (E.exists $ 
                E.from $ \aeml -> do 
                  E.where_ (paypal ^. PaypalToemail E.==. 
                            aeml E.?. AccountEmailEmail)
                  E.where_ $ E.in_ (aeml E.?. AccountEmailAccount) $
                    E.subList_select $ E.from $ \clubaccount -> do
                      E.where_ $ clubaccount ^. ClubAccountClub E.==. (E.val cid)
                      return $ E.just $ clubaccount ^. ClubAccountAccount)
      E.groupBy (email E.?. EmailId, paypal ^. PaypalName)
      E.orderBy [E.asc $ paypal ^. PaypalName]
      return (email E.?. EmailId, email E.?. EmailEmail, paypal ^. PaypalName, E.min_ (paypal ^. PaypalId))) 
  let chex = catMaybes $ (\(E.Value emlid, E.Value email, E.Value name, E.Value mbppid) -> 
               case mbppid of 
                 Just ppid -> Just (T.append name (T.append " " (fromMaybe "" email)), ppid)
                 Nothing -> Nothing) <$> membs
      ppids = catMaybes $ (\(_, _, _, E.Value ppid) -> 
               ppid) <$> membs
  return (chex, ppids)

defaultDues :: ClubId -> Handler [(Text, DuesRateId)]
defaultDues cid = do 
  dues <- runDB $ E.select $ E.from (\duesrate -> do 
    E.where_ $ duesrate ^. DuesRateClub E.==. (E.val cid)
    E.orderBy [E.asc $ duesrate ^. DuesRateAmount]
    return (duesrate ^. DuesRateId, duesrate ^. DuesRateName, duesrate ^. DuesRateAmount))
  return $ fmap (\(E.Value drid, E.Value drname, E.Value dramt) -> 
                  (drname `T.append` (T.pack (show dramt)), drid))
                dues

getCreatePaypalMembersR :: ClubId -> Handler Html
getCreatePaypalMembersR cid = do  
  (chex,ppids) <- makeChex cid
  (umform, umenc) <- generateFormPost $ userMakeForm chex (Just $ UserMake ppids)
  defaultLayout $ do [whamlet|
    cppuresult:
    <form method=post enctype=#{ umenc }>
      ^{umform}
      <input type=submit value=save>
    |]

postCreatePaypalMembersR :: ClubId -> Handler Html
postCreatePaypalMembersR cid = do 
  (chex,_) <- makeChex cid
  defdues <- defaultDues cid
  ((res,_),_) <- runFormPost $ userMakeForm chex Nothing
  case res of 
    FormFailure meh -> error $ show meh
    FormMissing -> error "form missing"
    FormSuccess umk -> do
      users <- makeUsers cid (paypals umk) 
      defaultLayout $ do [whamlet|
        created users:
        <br> 
          #{ show users }
        <br> from :
        <br>
          #{ show (paypals umk) }
        |]  

findPpDuesRateDB :: [Entity DuesRate] -> ClubId -> EmailId -> Handler [Centi]
findPpDuesRateDB drs cid eid = do 
  -- get all paypal transaction amounts from eid to club.
  ppts <- runDB $ E.select $ E.from $ \paypal -> do
    E.where_ $ (paypal ^. PaypalFromemail E.==. E.just (E.val eid)) 
    E.where_ $ E.in_ (paypal ^. PaypalToemail) (E.subList_select $ E.from (\clubmail -> do 
        E.where_ $ clubmail ^. ClubEmailClub E.==. E.val cid
        return $ E.just (clubmail ^. ClubEmailEmail)))
    E.orderBy [E.desc (paypal ^. PaypalDate)]
    return $ paypal ^. PaypalAmountGross
  return (fmap (\(E.Value a) -> a) ppts)

findPpDuesRate :: [Entity DuesRate] -> ClubId -> EmailId -> Handler (Maybe DuesRateId)
findPpDuesRate drs cid eid = do 
  -- get all paypal transaction amounts from eid to club.
  ppts <- runDB $ E.select $ E.from $ \paypal -> do
    E.where_ $ (paypal ^. PaypalFromemail E.==. E.just (E.val eid)) 
    E.where_ $ E.in_ (paypal ^. PaypalToemail) (E.subList_select $ E.from (\clubmail -> do 
        E.where_ $ clubmail ^. ClubEmailClub E.==. E.val cid
        return $ E.just (clubmail ^. ClubEmailEmail)))
    E.orderBy [E.desc (paypal ^. PaypalDate)]
    return $ paypal ^. PaypalAmountGross
  -- dues rate is the latest paypal payment that is in the rates list.
  let valids = fmap (findRate drs) (fmap (\(E.Value a) -> a) ppts)
  return $ listToMaybe $ catMaybes (fmap (fmap entityKey) valids)

findRate :: [Entity DuesRate] -> Centi -> Maybe (Entity DuesRate)
findRate rates amt = 
  let matches = filter (\(Entity id rt) -> (duesRateAmount rt) == amt) rates in
  listToMaybe matches

makeUsers :: ClubId -> [PaypalId] -> Handler [UserId]
makeUsers cid ppids = do 
  -- for each paypal transaction create a user record, 
  -- and a user account.
  -- user recs require a dues rate, but is that really necessary?
  curtime <- lift getCurrentTime
  (mapM (\pid -> makeUser cid pid curtime) ppids)

makeUser :: ClubId -> PaypalId -> UTCTime -> Handler UserId 
makeUser cid pid ct = do 
  mbpp <- runDB $ get pid
  pp <- unMaybeMsg mbpp "no paypal record!"
  ppfe <- unMaybeMsg (paypalFromemail pp) "no pp email"
  mbeml <- runDB $ get ppfe 
  eml <- unMaybeMsg mbeml "no email record!"
  addUser (User (emailEmail eml) (paypalName pp) Nothing (utctDay ct))
          ppfe
     
addUser :: User -> EmailId -> Handler UserId
addUser userrec eid = do 
  uid <- runDB $ insert $ userrec
  acctid <- runDB $ insert $ Account "dues"
  useracct <-runDB $ insert $ UserAccount uid acctid
  accteml <- runDB $ insert $ AccountEmail acctid eid
  return uid
 
