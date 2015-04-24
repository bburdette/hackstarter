module Handler.AddInternal where

import Import hiding ((==.), (!=.))
import Util
import Data.Fixed
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Database.Esqueleto
-- import qualified Database.Persist as P
import qualified Data.Text as T
-- import qualified Database.Persist as P
{-

what this should show:
  from account name.
  account owners
    clubs, projects, users.

  to account choices
    account name, account owners.
  
  amount.
  as-of datetime.    

-}

data AddInternalForm = AddInternalForm {
  aifToAccount :: AccountId,
  aifAmount :: Centi,
  aifDate :: Day,
  aifTime :: TimeOfDay 
  }

addInternalForm :: [(Text, AccountId)] -> Maybe UTCTime -> Form AddInternalForm 
addInternalForm toAccountChoices defaultDate = renderDivs $ AddInternalForm 
  <$> areq (selectFieldList toAccountChoices) "to account" Nothing
  <*> areq centiField "amount" Nothing
  <*> areq dayField "date" (utctDay <$> defaultDate)
  <*> areq timeField "time" ((localTimeOfDay . (utcToLocalTime utc)) <$> defaultDate)

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
  let mek = \(Value acctid, Value aname, Value uident) -> 
             (aname `T.append` " - " `T.append` uident, acctid)
  return $ mek <$> rawchoices
      
getAddInternalR :: AccountId -> Handler Html
getAddInternalR aid = do 
  mbacct <- runDB $ get aid
  acct <- unMaybeMsg mbacct "account not found"
  tochoices <- getUserAccountChoices aid
  now <- lift getCurrentTime
  (form,formenc) <- generateFormPost $ addInternalForm tochoices (Just now)
  defaultLayout $ do [whamlet|
    create internal transaction from account:
    <form method=post enctype=#{ formenc }>
      ^{ form }
      <input type=submit value=ok>
    |] 
    

{-
data AddInternalForm = AddInternalForm {
  aifToAccount :: AccountId,
  aifAmount :: Centi,
  aifDate :: Day,
  aifTime :: TimeOfDay 
  }
Internal
    fromaccount AccountId
    toaccount AccountId
    creator UserId
    date UTCTime default=CURRENT_TIMESTAMP
    amount Centi
    manual Bool 
Account
-}

postAddInternalR :: AccountId -> Handler Html
postAddInternalR aid = do
  login <- requireAuthId  
  tochoices <- getUserAccountChoices aid
  ((res, _),_) <- runFormPost $ addInternalForm tochoices Nothing
  case res of
    FormSuccess aif -> do 
      _ <- runDB $ insert $ Internal { 
        internalFromaccount = aid, 
        internalToaccount = aifToAccount aif, 
        internalCreator = login,
        internalDate = localTimeToUTC utc (LocalTime (aifDate aif) (aifTime aif)),
        internalAmount = aifAmount aif,
        internalManual = True }
      redirect $ AccountR aid
    _ -> error "fail"
        
    
