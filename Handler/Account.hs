module Handler.Account where

import Import
import Permissions
import TransactionQueries
import Data.List
import Data.Fixed
import Data.Maybe
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
      ppyls <- getAccountPaypals aid
      pis <- getAccountPaypalInternal aid
      internals <- getAccountInternals aid 
      defaultLayout $ do
        [whamlet| 
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
          <h3> paypal transactions
          <table class="low">
            <tr>
              <th> date 
              <th> gross
              <th> net
              <th> from
              <th> to 
              <th> name
              <th> description
              <th> memo
              <th> creator
            $forall (ppid, datetime, amountGross, amountNet, name, description, memo, fromEmail, toEmail, creatorId, creatorIdent) <- ppyls
              <tr>
                <td> #{show datetime}
                <td> #{show amountGross} 
                <td> #{show amountNet} 
                <td> #{fromMaybe "" fromEmail} 
                <td> #{fromMaybe "" toEmail} 
                <td> #{name}
                <td> #{description}
                <td> #{memo}
                <td> 
                  <a href=@{UserR creatorId}> #{ creatorIdent }
        |]

postAccountR :: AccountId -> Handler Html
postAccountR = error "Not yet implemented: postAccountR"
