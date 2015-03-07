module Handler.Ledger where

import Import
import Permissions
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

getLedgerR :: Handler Html
getLedgerR = do
  logid <- requireAuthId
  requireAdmin logid
  mahsums <- runDB $ E.select 
    $ E.from $ \lolwut -> do 
      let sumg = (E.sum_ (lolwut ^. LedgerAmountGross))
          sumn = (E.sum_ (lolwut ^. LedgerAmountNet))
      return (sumg, sumn)
  let (sumg, sumn) = case mahsums of 
                [(E.Value (Just gamt), E.Value (Just namt))] -> (gamt, namt)
                _ -> (0,0) :: (Int,Int)
   in do
    ledges <- runDB $ E.select 
      $ E.from $ \(E.InnerJoin (E.LeftOuterJoin ledger email) usercreator) -> do 
        E.on $ usercreator ^. UserId E.==. ledger ^. LedgerCreator
        E.on $ (ledger ^. LedgerFromemail E.==. email E.?. EmailId) 
        E.orderBy $ [E.asc ( ledger ^. LedgerDate)]
        return 
          ( ledger ^. LedgerDate,
            ledger ^. LedgerAmountGross,
            ledger ^. LedgerAmountNet,
            ledger ^. LedgerCreator,
            ledger ^. LedgerDescription,
            ledger ^. LedgerMemo,
            email E.?. EmailEmail,
            usercreator ^. UserIdent ) 
    defaultLayout $ do 
      [whamlet| 
        <h4> Ledger
        <br> Sum of transactions: #{show sumg} #{show sumn}
        <table class="ledgarrr">
          <tr>
            <th> Datetime
            <th> Gross
            <th> Net
            <th> Description
            <th> Memo
            <th> Email 
            <th> Creator
          $forall (E.Value datetime, E.Value gamount, E.Value namount, E.Value creator, E.Value description, E.Value memo, E.Value emailtxt, E.Value creatorIdent) <- ledges
            <tr>
              <td> #{ show datetime}
              <td> #{ show gamount }
              <td> #{ show namount }
              <td> #{ description }
              <td> #{ memo }
              <td> #{ maybe "" id emailtxt }
              <td> #{ creatorIdent }
      |]

postLedgerR :: Handler Html
postLedgerR = error "Not yet implemented: postLedgerR"
