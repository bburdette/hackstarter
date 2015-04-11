module Handler.CreatePaypalMembers where

import Import
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import Data.Maybe
import Data.Text as T
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
userMakeForm choisez mbum = renderDivs $ UserMake 
  <$> areq (checkboxesFieldList choisez) "valid users?" (paypals <$> mbum)

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
  ((res,_),_) <- runFormPost $ userMakeForm chex Nothing
  case res of 
    FormFailure meh -> error $ show meh
    FormMissing -> error "form missing"
    FormSuccess umk -> do 
      defaultLayout $ do [whamlet|
        #{ show (paypals umk) }
        |]  
