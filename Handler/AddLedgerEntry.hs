module Handler.AddLedgerEntry where

import Import
import Permissions
import Data.Time

data NewLedgerEntry = NewLedgerEntry {
  userName :: Text, 
  amount :: Int
  }
  
newLedgerEntryForm :: Maybe User -> Form NewLedgerEntry
newLedgerEntryForm mbusr = renderDivs $ NewLedgerEntry
  <$> areq textField ("user name: " { fsAttrs = [("readonly", "")] }) (userIdent <$> mbusr)
  <*> areq intField "amount" Nothing

getAddLedgerEntryR :: UserId -> Handler Html
getAddLedgerEntryR uid = do
  logid <- requireAuthId
  admin <- isAdmin logid
  case admin of
    False -> error "unauthorized"
    True -> do   
      mbuser <- runDB $ get uid
      case mbuser of 
        Nothing -> error "user record not found" 
        Just usr -> do 
          (widget, enctype) <- generateFormPost (newLedgerEntryForm (Just usr))
          defaultLayout $ [whamlet| 
            <form method=post enctype#{enctype}>
              ^{widget}
              <input type=submit value="Add transaction">
            |]
      
postAddLedgerEntryR :: UserId -> Handler Html
postAddLedgerEntryR uid = do
  logid <- requireAuthId
  admin <- isAdmin logid
  case admin of
    False -> error "unauthorized"
    True -> do   
      ((result, widget), enctype) <- runFormPost $ newLedgerEntryForm Nothing
      case result of 
        FormSuccess nle -> do 
          now <- lift getCurrentTime
          blah <- runDB $ insert $ Ledger uid (amount nle) logid now
          defaultLayout $ do
            [whamlet|insert result: #{show blah}|]
        _ -> error "fail"
