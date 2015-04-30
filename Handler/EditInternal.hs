module Handler.EditInternal where

import Import hiding ((==.), (!=.))
import Util
import Data.Fixed
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Database.Esqueleto
import TransactionQueries
import qualified Data.Text as T

data EditInternalForm = EditInternalForm {
  eifCreate :: Bool,
  eifTo :: Bool,
  eifAccount :: AccountId,
  eifAmount :: Centi,
  eifDay :: Day,
  eifTime :: TimeOfDay
  }

editInternalForm :: Bool -> [(Text, AccountId)] -> Maybe EditInternalForm -> Form EditInternalForm 
editInternalForm allowReplace accountChoices mbeif = renderDivs $ EditInternalForm 
  <$> (if allowReplace 
        then areq (selectFieldList [("replace transaction" :: Text, False), ("create transaction", True)]) "" (eifCreate <$> mbeif)
        else areq hiddenField "create transaction" (Just True))
  <*> areq (selectFieldList [("from" :: Text, False), ("to", True)]) "" (eifTo <$> mbeif)
  <*> areq (selectFieldList accountChoices) "account" (eifAccount <$> mbeif)
  <*> areq centiField "amount" (eifAmount <$> mbeif)
  <*> areq dayField "date" (eifDay <$> mbeif)
  <*> areq timeField "time" (eifTime <$> mbeif)


getEditInternalR :: AccountId -> InternalId -> Handler Html
getEditInternalR aid iid = do 
  mbacct <- runDB $ get aid
  acct <- unMaybeMsg mbacct "account not found"
  choices <- getUserAccountChoices aid
  mbint <- runDB $ get iid
  int <- unMaybeMsg mbint "internal transaction not found"
  let mbto = getIntTo int aid
  (tf,tfaid) <- unMaybeMsg mbto "invalid transaction from/to"
  let eif = EditInternalForm {
    eifCreate = False,
    eifTo = tf,
    eifAccount = tfaid,
    eifAmount = internalAmount int,
    eifDay = utctDay $ internalDate int,
    eifTime = (localTimeOfDay . (utcToLocalTime utc)) $ internalDate int
    }
  (form,formenc) <- generateFormPost $ 
    editInternalForm (internalManual int) choices (Just eif)
  defaultLayout $ do [whamlet|
    account: #{ accountName acct } 
    <form method=post enctype=#{formenc}>
      ^{form}
      <input type=submit value=ok>
    |]
    

getIntTo :: Internal -> AccountId -> Maybe (Bool, AccountId)
getIntTo int aid = 
  case (aid == (internalFromaccount int), aid == (internalToaccount int)) of 
    (True,False) -> Just (False, (internalToaccount int))
    (False,True) -> Just (True, (internalFromaccount int))
    (_,_) -> Nothing

postEditInternalR :: AccountId -> InternalId -> Handler Html
postEditInternalR = error "Not yet implemented: postEditInternalR"
