module Handler.UserTransactions where

import Import

getUserTransactionsR :: UserId -> Handler Html
getUserTransactionsR uid = do
  -- mbusr <- runDB $ get uid
  trannies <- runDB $ selectList [LedgerUser ==. uid] []
  defaultLayout $ do
    [whamlet| transactions:
      $forall (Entity key trans) <- trannies
        <li>
          #{ledgerAmount trans} #{show $ ledgerDate trans}
      |]

postUserTransactionsR :: UserId -> Handler Html
postUserTransactionsR = error "Not yet implemented: postUserTransactionsR"
