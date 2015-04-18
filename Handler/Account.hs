module Handler.Account where

import Import
import Permissions
import TransactionQueries
import Data.List
import Data.Fixed
import Data.Time.Clock
import Data.Time.Calendar
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))


getAccountR :: AccountId -> Handler Html
getAccountR aid = do
  logid <- requireAuthId
  admin <- isAdmin logid
  -- case admin || (logid == uid) of 
  case admin of 
    False -> error "unauthorized"
    True -> do
      -- mbusr <- runDB $ get uid
      -- mbdacct <- getDuesAccount uid
      pis <- getAccountPaypalInternal aid
      internals <- getAccountInternals aid 
      defaultLayout $ do
        [whamlet| 
          <h3> paypal->internal
          <table class="low">
            <tr>
              <th> from paypal
              <th> to account
              <th> created by
              <th> datetime
              <th> amount 
              <th> manual
            $forall (ppid, toacctid, toacctname, creatorid, creatorident, datetime, amount, manual) <- pis
              <tr>
                <td> #{ show ppid }
                <td> #{ toacctname } 
                <td> 
                  <a href=@{UserR creatorid}> #{ creatorident }
                <td> #{show datetime}
                <td> #{show amount} 
                <td> #{show manual} 
          <h3> internal->internal 
          <table class="low">
          <tr>
            <th> from account
            <th> to account 
            <th> creator
            <th> date
            <th> amount
            <th> manual
          $forall (id, fromaccount, fromAccountName, toaccount, toAccountName, creatorId, creatorIdent, datetime, amount, manual) <- internals
            <tr>
              <td> <a href=@{AccountR fromaccount}> #{ fromAccountName } 
              <td> <a href=@{AccountR toaccount}> #{ toAccountName } 
              <td> 
                <a href=@{UserR creatorId}> #{ creatorIdent }
              <td> #{show datetime}
              <td> #{show amount} 
              <td> #{show manual} 
        |]

postAccountR :: AccountId -> Handler Html
postAccountR = error "Not yet implemented: postAccountR"
