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
      $ E.from $ \(E.InnerJoin (E.InnerJoin user ledger) usercreator) -> do 
        E.on $ E.just (user ^. UserId) E.==. ledger ^. LedgerUser
        E.on $ usercreator ^. UserId E.==. ledger ^. LedgerCreator
        E.orderBy $ [E.asc ( ledger ^. LedgerDate)]
        return 
          ( user ^. UserId,
            user ^. UserIdent, 
            ledger ^. LedgerAmount,
            ledger ^. LedgerDate,
            ledger ^. LedgerCreator,
            usercreator ^. UserIdent ) 
    defaultLayout $ do 
      [whamlet| 
        <h4> Ledger
        <table class="ledgarrr">
          <tr>
            <th> User 
            <th> Amount 
            <th> Datetime
            <th> Creator
          $forall (E.Value usrId, E.Value usrident, E.Value amount, E.Value datetime, E.Value creator, E.Value creatorIdent) <- ledges
            <tr>
              <td> #{ usrident }
              <td> #{ show amount }
              <td> #{show $ datetime}
              <td> #{ creatorIdent }
          <br> Sum of transactions: #{show summ}
      |]

postLedgerR :: Handler Html
postLedgerR = error "Not yet implemented: postLedgerR"
