module TransactionQueries where
 
import Import hiding ((==.), (!=.), (||.), (&&.))
import Database.Esqueleto;
--import qualified Database.Persist as P
import Data.Fixed
import Data.Time.Clock
import Data.Text.Internal.Search as S
import qualified Data.Maybe as MB
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Util

-- get the first user account with 'dues' in the nam  
-- hopefully we'll replace this lame-o mechanism soon.
getUserDuesAccount :: UserId -> Handler (Maybe AccountId)
getUserDuesAccount uid = do 
  rawaccts <- runDB $ select $ from (\account -> do 
    where_ $ in_ (account ^. AccountId) (subList_select $ from 
      (\useracct -> do 
         where_ $ useracct ^. UserAccountUser ==. val uid
         return $ useracct ^. UserAccountAccount))
    return (account ^. AccountId, account ^. AccountName))
  -- does one account contain 'dues' in the name?
  let accts = (\(Value acctid, Value acctname) -> (acctid, acctname)) <$> rawaccts
      duesaccts = 
        filter (\(id,name) -> (not . null) (S.indices "dues" (T.toLower name))) accts
  return $ fst <$> MB.listToMaybe duesaccts

-- club accounts and user accounts, excluding the passed in account id.
getUserAccountChoices :: AccountId -> Handler [(Text, AccountId)]
getUserAccountChoices aid = do 
  mbacct <- runDB $ get aid
  acct <- unMaybe mbacct
  let aclub = accountClub acct
  rawchoices <- runDB $ select $ from 
    (\(InnerJoin (InnerJoin account useraccount) user) -> do
      on $ useraccount ^. UserAccountUser ==. user ^. UserId
      on $ account ^. AccountId ==. useraccount ^. UserAccountAccount
      where_ $ (account ^. AccountClub ==. val aclub) &&.
               (account ^. AccountId !=. val aid)
      orderBy [asc (user ^. UserIdent), asc (account ^. AccountName)]
      return (account ^. AccountId, account ^. AccountName, user ^. UserIdent))
  rawclubchoices <- runDB $ select $ from 
    (\(InnerJoin (InnerJoin account clubaccount) club) -> do
      on $ clubaccount ^. ClubAccountClub ==. club ^. ClubId
      on $ account ^. AccountId ==. clubaccount ^. ClubAccountAccount
      where_ $ (account ^. AccountClub ==. val aclub) &&.
               (account ^. AccountId !=. val aid)
      orderBy [asc (club ^. ClubName), asc (account ^. AccountName)]
      return (account ^. AccountId, account ^. AccountName, club ^. ClubName))
  let mek = \(Value acctid, Value aname, Value ident) -> 
             (ident `T.append` " - " `T.append` aname, acctid)
  return $ mek <$> (rawclubchoices ++ rawchoices)

{-
getClubAccountChoices :: ClubId -> Handler [(Text, AccountId)]
getClubAccountChoices cid = do 
  rawchoices <- runDB $ select $ from 
    (\(InnerJoin (InnerJoin account useraccount) user) -> do
      on $ useraccount ^. UserAccountUser ==. user ^. UserId
      on $ account ^. AccountId ==. useraccount ^. UserAccountAccount
      where_ $ (account ^. AccountClub ==. val aclub) 
      orderBy [asc (user ^. UserIdent), asc (account ^. AccountName)]
      return (account ^. AccountId, account ^. AccountName, user ^. UserIdent))
  rawclubchoices <- runDB $ select $ from 
    (\(InnerJoin (InnerJoin account clubaccount) club) -> do
      on $ clubaccount ^. ClubAccountClub ==. club ^. ClubId
      on $ account ^. AccountId ==. clubaccount ^. ClubAccountAccount
      where_ $ (account ^. AccountClub ==. val aclub) 
      orderBy [asc (club ^. ClubName), asc (account ^. AccountName)]
      return (account ^. AccountId, account ^. AccountName, club ^. ClubName))
  let mek = \(Value acctid, Value aname, Value ident) -> 
             (ident `T.append` " - " `T.append` aname, acctid)
  return $ mek <$> (rawclubchoices ++ rawchoices)
-}

----------------------------------------------------------
-- get all transactions
----------------------------------------------------------

getPaypals :: Handler [(PaypalId, UTCTime, Centi, Centi, Text, Text, Text, Maybe Text, Maybe Text, UserId, Text)]
getPaypals = do 
  paypals <- runDB $ select $ from $ 
    \(InnerJoin (LeftOuterJoin (LeftOuterJoin paypal email) email2) usercreator) -> do 
      on $ usercreator ^. UserId ==. paypal ^. PaypalCreator
      on (paypal ^. PaypalToemail ==. email2 ?. EmailId)
      on (paypal ^. PaypalFromemail ==. email ?. EmailId)
      orderBy $ [asc ( paypal ^. PaypalDate)]
      return 
        ( paypal ^. PaypalId,
          paypal ^. PaypalDate,
          paypal ^. PaypalAmountGross,
          paypal ^. PaypalAmountNet,
          paypal ^. PaypalName,
          paypal ^. PaypalDescription,
          paypal ^. PaypalMemo,
          email ?. EmailEmail,
          email2 ?. EmailEmail,
          paypal ^. PaypalCreator,
          usercreator ^. UserIdent ) 
  return $ (\(Value a, Value b, Value c, Value d, Value e, Value f, Value g, Value h, Value i, Value j, Value k) -> (a,b,c,d,e,f,g,h,i,j,k)) <$> paypals

getPaypalInternal :: Handler [(PaypalId, AccountId, Text, UserId, Text, UTCTime, Centi, Bool)]
getPaypalInternal = do 
  ppis <- runDB $ select $ from $
    \(InnerJoin (InnerJoin ppi user) account) -> do
      on $ ppi ^. PaypalInternalToaccount ==. account ^. AccountId 
      on $ ppi ^. PaypalInternalCreator ==. user ^. UserId
      orderBy [asc (ppi ^. PaypalInternalDate)]
      return (ppi ^. PaypalInternalFrompaypal, 
              ppi ^. PaypalInternalToaccount,
              account ^. AccountName,
              ppi ^. PaypalInternalCreator,
              user ^. UserIdent,
              ppi ^. PaypalInternalDate,
              ppi ^. PaypalInternalAmount,
              ppi ^. PaypalInternalManual)
  return $ (\(Value a, Value b, Value c, Value d, Value e, Value f, Value g, Value h) -> (a,b,c,d,e,f,g,h)) <$> ppis 

getInternals :: Handler [(InternalId, AccountId, Text, AccountId, Text, UserId, Text, UTCTime, Centi, Bool)]
getInternals = do 
  internals <- runDB $ select $ from $ 
    \(InnerJoin (InnerJoin (InnerJoin internal user) frmacct) toacct) -> do
      on $ internal ^. InternalToaccount ==. toacct ^. AccountId 
      on $ internal ^. InternalFromaccount ==. frmacct ^. AccountId
      on $ internal ^. InternalCreator ==. user ^. UserId
      orderBy [asc $ internal ^. InternalDate]
      return (internal ^. InternalId, 
              internal ^. InternalFromaccount, 
              frmacct ^. AccountName,
              internal ^. InternalToaccount,
              toacct ^. AccountName,
              internal ^. InternalCreator,
              user ^. UserIdent,
              internal ^. InternalDate,
              internal ^. InternalAmount,
              internal ^. InternalManual)
  return $ (\(Value a, Value b, Value c, Value d, Value e, Value f, Value g, Value h, Value i, Value j) -> (a,b,c,d,e,f,g,h,i,j)) <$> internals 

----------------------------------------------------------
-- get transactions for one user.
----------------------------------------------------------

getAccountPaypals :: AccountId -> Handler [(PaypalId, UTCTime, Centi, Centi, Text, Text, Text, Maybe Text, Maybe Text, UserId, Text)]
getAccountPaypals acctid = do
  -- there should be a better way than repeating that sublist query!
  paypals <- runDB $ select $ from $ 
    \(InnerJoin (LeftOuterJoin (LeftOuterJoin paypal email) email2) usercreator) -> do 
      on $ usercreator ^. UserId ==. paypal ^. PaypalCreator
      on (paypal ^. PaypalToemail ==. email2 ?. EmailId)
      on (paypal ^. PaypalFromemail ==. email ?. EmailId)
      where_ $ (in_ (paypal ^. PaypalFromemail) 
                       (subList_select $ from 
                         (\acctemail -> do 
                           where_ $ acctemail ^. AccountEmailAccount ==. (val acctid)
                           return $ just (acctemail ^. AccountEmailEmail))))
           ||. (in_ (paypal ^. PaypalToemail) 
                       (subList_select $ from 
                         (\acctemail -> do 
                           where_ $ acctemail ^. AccountEmailAccount ==. (val acctid)
                           return $ just (acctemail ^. AccountEmailEmail))))
      orderBy $ [asc ( paypal ^. PaypalDate)]
      return 
        ( paypal ^. PaypalId,
          paypal ^. PaypalDate,
          paypal ^. PaypalAmountGross,
          paypal ^. PaypalAmountNet,
          paypal ^. PaypalName,
          paypal ^. PaypalDescription,
          paypal ^. PaypalMemo,
          email ?. EmailEmail,
          email2 ?. EmailEmail,
          paypal ^. PaypalCreator,
          usercreator ^. UserIdent ) 
  return $ (\(Value a, Value b, Value c, Value d, Value e, Value f, Value g, Value h, Value i, Value j, Value k) -> (a,b,c,d,e,f,g,h,i,j,k)) <$> paypals

getAccountPaypalInternal :: AccountId -> Handler [(PaypalId, AccountId, Text, UserId, Text, UTCTime, Centi, Bool)]
getAccountPaypalInternal aid = do 
  ppis <- runDB $ select $ from $
    \(InnerJoin (InnerJoin ppi user) account) -> do
      on $ ppi ^. PaypalInternalToaccount ==. account ^. AccountId 
      on $ ppi ^. PaypalInternalCreator ==. user ^. UserId
      where_ $ ppi ^. PaypalInternalToaccount ==. (val aid)
      orderBy [asc $ ppi ^. PaypalInternalDate]
      return (ppi ^. PaypalInternalFrompaypal, 
              ppi ^. PaypalInternalToaccount,
              account ^. AccountName,
              ppi ^. PaypalInternalCreator,
              user ^. UserIdent,
              ppi ^. PaypalInternalDate,
              ppi ^. PaypalInternalAmount,
              ppi ^. PaypalInternalManual)
  return $ (\(Value a, Value b, Value c, Value d, Value e, Value f, Value g, Value h) -> (a,b,c,d,e,f,g,h)) <$> ppis 

getAccountInternals :: AccountId -> Handler [(InternalId, AccountId, Text, Maybe Text, AccountId, Text, Maybe Text, UserId, Text, UTCTime, Centi, Bool)]
getAccountInternals aid = do 
  internals <- runDB $ select $ from $ 
    \(InnerJoin (InnerJoin (InnerJoin internal user) frmacct) toacct) -> do
      on $ internal ^. InternalToaccount ==. toacct ^. AccountId 
      on $ internal ^. InternalFromaccount ==. frmacct ^. AccountId
      on $ internal ^. InternalCreator ==. user ^. UserId
      where_ $ (internal ^. InternalToaccount ==. (val aid)) 
        ||. (internal ^. InternalFromaccount ==. (val aid))
      orderBy [asc $ internal ^. InternalDate]
      return (internal ^. InternalId, 
              internal ^. InternalFromaccount, 
              frmacct ^. AccountName,
              internal ^. InternalToaccount,
              toacct ^. AccountName,
              internal ^. InternalCreator,
              user ^. UserIdent,
              internal ^. InternalDate,
              internal ^. InternalAmount,
              internal ^. InternalManual)
  let prelim = (\(Value a, Value b, Value c, Value d, Value e, Value f, Value g, Value h, Value i, Value j) -> (a,b,c,d,e,f,g,h,i,j)) <$> internals 
      fromaccts = (\(_,a,_,_,_,_,_,_,_,_) -> a) <$> prelim
      toaccts = (\(_,_,_,a,_,_,_,_,_,_) -> a) <$> prelim
      allaccts = Set.toList $ Set.fromList $ fromaccts ++ toaccts
  ownertext <- mapM getAccountOwners allaccts 
  let ot = zip allaccts ownertext
      results = (\(a,to,c,from,e,f,g,h,i,j) -> (a,to,c,lookup to ot, from,e,lookup from ot, f,g,h,i,j)) <$> prelim
  return results

-- get only manual transactions from/to fromaid.
-- mark the ones to toaid with true. 
getAccountInternalsFromTo :: AccountId -> AccountId -> Handler [(UTCTime, Centi, Bool)]
getAccountInternalsFromTo fromaid toaid = do 
  internals <- runDB $ select $ from $ 
    \internal -> do
      where_ $ (internal ^. InternalFromaccount ==. (val fromaid)
           ||. internal ^. InternalToaccount ==. (val fromaid))
           &&. internal ^. InternalManual ==. (val True) 
      orderBy [asc $ internal ^. InternalDate]
      return (internal ^. InternalFromaccount ==. (val fromaid),
              internal ^. InternalDate,
              internal ^. InternalAmount,
              internal ^. InternalToaccount ==. (val toaid))
  return $ (\(Value f, Value a, Value b, Value c) -> (a,inv b f,c)) <$> internals 
    where inv num bool = if bool then -num else num

getAccountOwners :: AccountId -> Handler Text
getAccountOwners aid = do
  clubs <- runDB $ select $ from (\(InnerJoin club clubaccount) -> do
    on $ club ^. ClubId ==. clubaccount ^. ClubAccountClub
    where_ $ clubaccount ^. ClubAccountAccount ==. (val aid)
    return $ club ^. ClubName)
  users <- runDB $ select $ from (\(InnerJoin user useraccount) -> do
    on $ user ^. UserId ==. useraccount ^. UserAccountUser
    where_ $ useraccount ^. UserAccountAccount ==. (val aid)
    return $ user ^. UserIdent)
  return $ T.intercalate ", " $ 
    ((\(Value a) -> a) <$> users) ++ ((\(Value a) -> a) <$> clubs)

getPaypal :: PaypalId -> Handler (Maybe (UTCTime, Centi, Centi, Text, Text, Text, Maybe Text, Maybe Text, UserId, Text))
getPaypal pid = do
  -- there should be a better way than repeating that sublist query!
  paypals <- runDB $ select $ from $ 
    \(InnerJoin (LeftOuterJoin (LeftOuterJoin paypal email) email2) usercreator) -> do 
      on $ usercreator ^. UserId ==. paypal ^. PaypalCreator
      on (paypal ^. PaypalToemail ==. email2 ?. EmailId)
      on (paypal ^. PaypalFromemail ==. email ?. EmailId)
      where_ $ paypal ^. PaypalId ==. (val pid)
      return 
        ( paypal ^. PaypalDate,
          paypal ^. PaypalAmountGross,
          paypal ^. PaypalAmountNet,
          paypal ^. PaypalName,
          paypal ^. PaypalDescription,
          paypal ^. PaypalMemo,
          email ?. EmailEmail,
          email2 ?. EmailEmail,
          paypal ^. PaypalCreator,
          usercreator ^. UserIdent ) 
  return $ MB.listToMaybe $ (\(Value a, Value b, Value c, Value d, Value e, Value f, Value g, Value h, Value i, Value j) -> (a,b,c,d,e,f,g,h,i,j)) <$> paypals

getPaypalPaypalInternal :: PaypalId -> Handler [(PaypalId, AccountId, Text, UserId, Text, UTCTime, Centi, Bool)]
getPaypalPaypalInternal pid = do 
  ppis <- runDB $ select $ from $
    \(InnerJoin (InnerJoin ppi user) account) -> do
      on $ ppi ^. PaypalInternalToaccount ==. account ^. AccountId 
      on $ ppi ^. PaypalInternalCreator ==. user ^. UserId
      where_ $ ppi ^. PaypalInternalFrompaypal ==. (val pid)
      orderBy [asc $ ppi ^. PaypalInternalDate]
      return (ppi ^. PaypalInternalFrompaypal, 
              ppi ^. PaypalInternalToaccount,
              account ^. AccountName,
              ppi ^. PaypalInternalCreator,
              user ^. UserIdent,
              ppi ^. PaypalInternalDate,
              ppi ^. PaypalInternalAmount,
              ppi ^. PaypalInternalManual)
  return $ (\(Value a, Value b, Value c, Value d, Value e, Value f, Value g, Value h) -> (a,b,c,d,e,f,g,h)) <$> ppis 


