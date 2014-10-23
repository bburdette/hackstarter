module Handler.Ledger where

import Import

getLedgerR :: Handler Html
getLedgerR = do
  ledges <- runDB $ selectList [] []
  defaultLayout $ do 
    [whamlet| 
      <h4> Ledger
      <table class="ledger">
        <tr>
          <th> User 
          <th> Amount 
          <th> Datetime
        $forall (Entity id ledge) <- ledges
          <tr>
            <td> #{show $ ledgerUser ledge}
            <td> #{ledgerAmount ledge}
            <td> #{show $ ledgerDate ledge}
    |]

postLedgerR :: Handler Html
postLedgerR = error "Not yet implemented: postLedgerR"
