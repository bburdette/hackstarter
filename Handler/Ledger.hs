module Handler.Ledger where

import Import
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

{-
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
-}

getLedgerR :: Handler Html
getLedgerR = do
  ledges <- runDB $ E.select 
    $ E.from $ \(E.InnerJoin user ledger) -> do 
      E.on $ user ^. UserId E.==. ledger ^. LedgerUser
      return 
        ( user ^. UserId,
          user ^. UserIdent, 
          ledger ^. LedgerAmount,
          ledger ^. LedgerDate ) 
  defaultLayout $ do 
    [whamlet| 
      <h4> Ledger
      <table class="ledgarrr">
        <tr>
          <th> User 
          <th> Amount 
          <th> Datetime
        $forall (E.Value usrId, E.Value usrident, E.Value amount, E.Value datetime) <- ledges
          <tr>
            <td> #{ usrident }
            <td> #{ show amount }
            <td> #{show $ datetime}
    |]

postLedgerR :: Handler Html
postLedgerR = error "Not yet implemented: postLedgerR"
