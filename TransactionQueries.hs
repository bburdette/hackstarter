module TransactionQueries where
 
import Import
--import Permissions
--import Data.List
import Data.Fixed
--import Data.Text
import Data.Time.Clock
import Data.Text.Internal.Search as S
import qualified Data.Maybe as MB
import qualified Data.Text as T
-- import Data
--import Data.Time.Calendar
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

-- get the first user account with 'dues' in the name.  
-- hopefully we'll replace this lame-o mechanism soon.
getDuesAccount :: UserId -> Handler (Maybe AccountId)
getDuesAccount uid = do 
  rawaccts <- runDB $ E.select $ E.from (\account -> do 
    E.where_ $ E.in_ (account ^. AccountId) (E.subList_select $ E.from 
      (\useracct -> do 
         E.where_ $ useracct ^. UserAccountUser E.==. E.val uid
         return $ useracct ^. UserAccountAccount))
    return (account ^. AccountId, account ^. AccountName))
  -- does one account contain 'dues' in the name?
  let accts = (\(E.Value acctid, E.Value acctname) -> (acctid, acctname)) <$> rawaccts
      duesaccts = 
        filter (\(id,name) -> (not . null) (S.indices "dues" (T.toLower name))) accts
  return $ fst <$> MB.listToMaybe duesaccts

----------------------------------------------------------
-- get all transactions
----------------------------------------------------------

getPaypals :: Handler [(PaypalId, UTCTime, Centi, Centi, Text, Text, Text, Maybe Text, Maybe Text, UserId, Text)]
getPaypals = do 
  paypals <- runDB $ E.select $ E.from $ 
    \(E.InnerJoin (E.LeftOuterJoin (E.LeftOuterJoin paypal email) email2) usercreator) -> do 
      E.on $ usercreator ^. UserId E.==. paypal ^. PaypalCreator
      E.on (paypal ^. PaypalToemail E.==. email2 E.?. EmailId)
      E.on (paypal ^. PaypalFromemail E.==. email E.?. EmailId)
      E.orderBy $ [E.asc ( paypal ^. PaypalDate)]
      return 
        ( paypal ^. PaypalId,
          paypal ^. PaypalDate,
          paypal ^. PaypalAmountGross,
          paypal ^. PaypalAmountNet,
          paypal ^. PaypalName,
          paypal ^. PaypalDescription,
          paypal ^. PaypalMemo,
          email E.?. EmailEmail,
          email2 E.?. EmailEmail,
          paypal ^. PaypalCreator,
          usercreator ^. UserIdent ) 
  return $ (\(E.Value a, E.Value b, E.Value c, E.Value d, E.Value e, E.Value f, E.Value g, E.Value h, E.Value i, E.Value j, E.Value k) -> (a,b,c,d,e,f,g,h,i,j,k)) <$> paypals

getPaypalInternal :: Handler [(PaypalId, AccountId, Text, UserId, Text, UTCTime, Centi, Bool)]
getPaypalInternal = do 
  ppis <- runDB $ E.select $ E.from $
    \(E.InnerJoin (E.InnerJoin ppi user) account) -> do
      E.on $ ppi ^. PaypalInternalToaccount E.==. account ^. AccountId 
      E.on $ ppi ^. PaypalInternalCreator E.==. user ^. UserId
      return (ppi ^. PaypalInternalFrompaypal, 
              ppi ^. PaypalInternalToaccount,
              account ^. AccountName,
              ppi ^. PaypalInternalCreator,
              user ^. UserIdent,
              ppi ^. PaypalInternalDate,
              ppi ^. PaypalInternalAmount,
              ppi ^. PaypalInternalManual)
  return $ (\(E.Value a, E.Value b, E.Value c, E.Value d, E.Value e, E.Value f, E.Value g, E.Value h) -> (a,b,c,d,e,f,g,h)) <$> ppis 

getInternals :: Handler [(InternalId, AccountId, Text, AccountId, Text, UserId, Text, UTCTime, Centi, Bool)]
getInternals = do 
  internals <- runDB $ E.select $ E.from $ 
    \(E.InnerJoin (E.InnerJoin (E.InnerJoin internal user) frmacct) toacct) -> do
      E.on $ internal ^. InternalToaccount E.==. toacct ^. AccountId 
      E.on $ internal ^. InternalFromaccount E.==. frmacct ^. AccountId
      E.on $ internal ^. InternalCreator E.==. user ^. UserId
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
  return $ (\(E.Value a, E.Value b, E.Value c, E.Value d, E.Value e, E.Value f, E.Value g, E.Value h, E.Value i, E.Value j) -> (a,b,c,d,e,f,g,h,i,j)) <$> internals 

----------------------------------------------------------
-- get transactions for one user.
----------------------------------------------------------

getAccountPaypals :: AccountId -> Handler [(PaypalId, UTCTime, Centi, Centi, Text, Text, Text, Maybe Text, Maybe Text, UserId, Text)]
getAccountPaypals acctid = do
  -- there should be a better way than repeating that sublist query!
  paypals <- runDB $ E.select $ E.from $ 
    \(E.InnerJoin (E.LeftOuterJoin (E.LeftOuterJoin paypal email) email2) usercreator) -> do 
      E.on $ usercreator ^. UserId E.==. paypal ^. PaypalCreator
      E.on (paypal ^. PaypalToemail E.==. email2 E.?. EmailId)
      E.on (paypal ^. PaypalFromemail E.==. email E.?. EmailId)
      E.where_ $ (E.in_ (paypal ^. PaypalFromemail) 
                       (E.subList_select $ E.from 
                         (\acctemail -> do 
                           E.where_ $ acctemail ^. AccountEmailAccount E.==. (E.val acctid)
                           return $ E.just (acctemail ^. AccountEmailEmail))))
           E.||. (E.in_ (paypal ^. PaypalToemail) 
                       (E.subList_select $ E.from 
                         (\acctemail -> do 
                           E.where_ $ acctemail ^. AccountEmailAccount E.==. (E.val acctid)
                           return $ E.just (acctemail ^. AccountEmailEmail))))
      E.orderBy $ [E.asc ( paypal ^. PaypalDate)]
      return 
        ( paypal ^. PaypalId,
          paypal ^. PaypalDate,
          paypal ^. PaypalAmountGross,
          paypal ^. PaypalAmountNet,
          paypal ^. PaypalName,
          paypal ^. PaypalDescription,
          paypal ^. PaypalMemo,
          email E.?. EmailEmail,
          email2 E.?. EmailEmail,
          paypal ^. PaypalCreator,
          usercreator ^. UserIdent ) 
  return $ (\(E.Value a, E.Value b, E.Value c, E.Value d, E.Value e, E.Value f, E.Value g, E.Value h, E.Value i, E.Value j, E.Value k) -> (a,b,c,d,e,f,g,h,i,j,k)) <$> paypals

getAccountPaypalInternal :: AccountId -> Handler [(PaypalId, AccountId, Text, UserId, Text, UTCTime, Centi, Bool)]
getAccountPaypalInternal aid = do 
  ppis <- runDB $ E.select $ E.from $
    \(E.InnerJoin (E.InnerJoin ppi user) account) -> do
      E.on $ ppi ^. PaypalInternalToaccount E.==. account ^. AccountId 
      E.on $ ppi ^. PaypalInternalCreator E.==. user ^. UserId
      E.where_ $ ppi ^. PaypalInternalToaccount E.==. (E.val aid)
      return (ppi ^. PaypalInternalFrompaypal, 
              ppi ^. PaypalInternalToaccount,
              account ^. AccountName,
              ppi ^. PaypalInternalCreator,
              user ^. UserIdent,
              ppi ^. PaypalInternalDate,
              ppi ^. PaypalInternalAmount,
              ppi ^. PaypalInternalManual)
  return $ (\(E.Value a, E.Value b, E.Value c, E.Value d, E.Value e, E.Value f, E.Value g, E.Value h) -> (a,b,c,d,e,f,g,h)) <$> ppis 

getAccountInternals :: AccountId -> Handler [(InternalId, AccountId, Text, AccountId, Text, UserId, Text, UTCTime, Centi, Bool)]
getAccountInternals aid = do 
  internals <- runDB $ E.select $ E.from $ 
    \(E.InnerJoin (E.InnerJoin (E.InnerJoin internal user) frmacct) toacct) -> do
      E.on $ internal ^. InternalToaccount E.==. toacct ^. AccountId 
      E.on $ internal ^. InternalFromaccount E.==. frmacct ^. AccountId
      E.on $ internal ^. InternalCreator E.==. user ^. UserId
      E.where_ $ (internal ^. InternalToaccount E.==. (E.val aid)) 
        E.||. (internal ^. InternalFromaccount E.==. (E.val aid))
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
  return $ (\(E.Value a, E.Value b, E.Value c, E.Value d, E.Value e, E.Value f, E.Value g, E.Value h, E.Value i, E.Value j) -> (a,b,c,d,e,f,g,h,i,j)) <$> internals 

