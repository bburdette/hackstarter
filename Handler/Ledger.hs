module Handler.Ledger where

import Import
import Permissions
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

getLedgerR :: Handler Html
getLedgerR = do
  logid <- requireAuthId
  admin <- isAdmin logid
  mahsum <- runDB $ E.select 
    $ E.from $ \lolwut -> do 
      let sumamt = (E.sum_ (lolwut ^. LedgerAmount))
      return sumamt
  let summ = case mahsum of 
                [E.Value (Just amt)] -> amt
                _ -> 0 :: Int
   in do
    ledges <- runDB $ E.select 
      $ E.from $ \(E.InnerJoin user ledger) -> do 
        E.on $ user ^. UserId E.==. ledger ^. LedgerUser
        E.orderBy $ [E.asc ( ledger ^. LedgerDate)]
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
          <br> Sum of transactions: #{show summ}
      |]

postLedgerR :: Handler Html
postLedgerR = error "Not yet implemented: postLedgerR"
