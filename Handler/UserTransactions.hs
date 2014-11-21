module Handler.UserTransactions where

import Import
import Permissions
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

getUserBalance :: UserId -> Handler Int
getUserBalance uid = do
  (runDB $ E.select 
    $ E.from $ \lolwut -> do 
      let sumamt = (E.sum_ (lolwut ^. LedgerAmount))
      E.where_ $ lolwut ^. LedgerUser E.==. (E.val $ Just uid)
      return sumamt) >>= (\x -> 
        case x of 
          [E.Value (Just amt)] -> return amt
          _ -> return 0) 

getUserTransactionsR :: UserId -> Handler Html
getUserTransactionsR uid = do
  logid <- requireAuthId
  admin <- isAdmin logid
  case admin || (logid == uid) of 
    False -> error "unauthorized"
    True -> do
      mbusr <- runDB $ get uid
      bal <- getUserBalance uid
      case mbusr of 
        Nothing -> error "invalid user id"
        Just usr -> do 
          ledges <- runDB $ E.select 
            $ E.from $ \(E.InnerJoin user ledger) -> do 
              E.where_ $ ledger ^. LedgerUser E.==. (E.val $ Just uid)
              E.on $ user ^. UserId E.==. ledger ^. LedgerCreator
              E.orderBy $ [E.asc ( ledger ^. LedgerDate)]
              return 
                ( ledger ^. LedgerAmount,
                  ledger ^. LedgerDate,
                  ledger ^. LedgerCreator,
                  user ^. UserIdent ) 
          defaultLayout $ do
            [whamlet| 
              <h3> transactions for user: 
                <a href=@{UserR uid}>#{userIdent usr} 
              <br>
                <a href=@{AddLedgerEntryR uid}> add transaction 
              <table class="sum">
                <tr>
                  <th> balance
                <tr>
                  <td> #{show bal}
               <table class="low">
                <tr>
                  <th> amount
                  <th> datetime
                  <th> created by
                $forall (E.Value amount, E.Value datetime, E.Value creatorid, E.Value creatorIdent) <- ledges
                  <tr>
                    <td> #{amount}
                    <td> #{show datetime}
                    <td> #{creatorIdent}
             |]

postUserTransactionsR :: UserId -> Handler Html
postUserTransactionsR = error "Not yet implemented: postUserTransactionsR"
