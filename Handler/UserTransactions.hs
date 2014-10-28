module Handler.UserTransactions where

import Import
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

getUserTransactionsR :: UserId -> Handler Html
getUserTransactionsR uid = do
  mbusr <- runDB $ get uid
  case mbusr of 
    Nothing -> error "invalid user id"
    Just usr -> do 
      [E.Value (Just amt)] <- runDB $ E.select 
        $ E.from $ \lolwut -> do 
          let sumamt = (E.sum_ (lolwut ^. LedgerAmount))
          E.where_ $ lolwut ^. LedgerUser E.==. (E.val uid)
          return sumamt
      let usrsum = amt :: Int in do
        ledges <- runDB $ selectList [LedgerUser ==. uid] []
        defaultLayout $ do
          [whamlet| 
            <h3> transactions for user: 
              <a href=@{UserR uid}>#{userIdent usr} 
            <table class="sum">
              <tr>
                <th> balance
              <tr>
                <td> #{show usrsum}
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
