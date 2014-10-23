module Handler.UserTransactions where

import Import

getUserTransactionsR :: UserId -> Handler Html
getUserTransactionsR uid = do
  mbusr <- runDB $ get uid
  case mbusr of 
    Nothing -> error "invalid user id"
    Just usr -> do 
      ledges <- runDB $ selectList [LedgerUser ==. uid] []
      defaultLayout $ do
        [whamlet| 
          <h3> transactions for user: 
            <a href=@{UserR uid}>#{userIdent usr} 
          <table class="low">
            <tr>
              <th> amount
              <th> datetime
            $forall (Entity key ledge) <- ledges
              <tr>
                <td> #{ledgerAmount ledge} 
                <td> #{show $ ledgerDate ledge}
          |]

postUserTransactionsR :: UserId -> Handler Html
postUserTransactionsR = error "Not yet implemented: postUserTransactionsR"
