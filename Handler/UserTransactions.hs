module Handler.UserTransactions where

import Import
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

getUserBalance :: UserId -> Handler Int
getUserBalance uid = do
  (runDB $ E.select 
    $ E.from $ \lolwut -> do 
      let sumamt = (E.sum_ (lolwut ^. LedgerAmount))
      E.where_ $ lolwut ^. LedgerUser E.==. (E.val uid)
      return sumamt) >>= (\x -> 
        case x of 
          [E.Value (Just amt)] -> return amt
          _ -> return 0) 

getUserTransactionsR :: UserId -> Handler Html
getUserTransactionsR uid = do
  mbusr <- runDB $ get uid
  bal <- getUserBalance uid
  case mbusr of 
    Nothing -> error "invalid user id"
    Just usr -> do 
      ledges <- runDB $ selectList [LedgerUser ==. uid] []
      defaultLayout $ do
        [whamlet| 
          <h3> transactions for user: 
            <a href=@{UserR uid}>#{userIdent usr} 
          <table class="sum">
            <tr>
              <th> balance
            <tr>
              <td> #{show bal}
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
