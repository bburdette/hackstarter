module Handler.Ledger where

import Import
import Permissions
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

getLedgerR :: Handler Html
getLedgerR = do
  logid <- requireAuthId
  requireAdmin logid
  mahsum <- runDB $ E.select 
    $ E.from $ \lolwut -> do 
      let sumamt = (E.sum_ (lolwut ^. LedgerAmountGross))
      return sumamt
  let summ = case mahsum of 
                [E.Value (Just amt)] -> amt
                _ -> 0 :: Int
   in do
    ledges <- runDB $ E.select 
      $ E.from $ \(E.InnerJoin (E.LeftOuterJoin (E.LeftOuterJoin ledger user) email) usercreator) -> do 
        E.on $ usercreator ^. UserId E.==. ledger ^. LedgerCreator
        E.on $ (ledger ^. LedgerEmail E.==. email E.?. EmailId) 
        E.on $ (ledger ^. LedgerUser E.==. user E.?. UserId) 
        E.orderBy $ [E.asc ( ledger ^. LedgerDate)]
        return 
          ( user E.?. UserId,
            user E.?. UserIdent,
            ledger ^. LedgerAmountGross,
            ledger ^. LedgerAmountNet,
            ledger ^. LedgerDate,
            ledger ^. LedgerCreator,
            email E.?. EmailEmail,
            usercreator ^. UserIdent ) 
    defaultLayout $ do 
      [whamlet| 
        <h4> Ledger
        <table class="ledgarrr">
          <tr>
            <th> User 
            <th> Gross
            <th> Net
            <th> Datetime
            <th> Email 
            <th> Creator
          $forall (E.Value usrId, E.Value usrident, E.Value gamount, E.Value namount, E.Value datetime, E.Value creator, E.Value emailtxt, E.Value creatorIdent) <- ledges
            <tr>
              <td> 
                $maybe uid <- usrId
                  <a href=@{UserR uid}> #{ maybe "" id usrident }
                $nothing
                  #{ maybe "" id usrident }
              <td> #{ show gamount }
              <td> #{ show namount }
              <td> #{ show datetime}
              <td> #{ maybe "" id emailtxt }
              <td> #{ creatorIdent }
          <br> Sum of transactions: #{show summ}
      |]

postLedgerR :: Handler Html
postLedgerR = error "Not yet implemented: postLedgerR"
