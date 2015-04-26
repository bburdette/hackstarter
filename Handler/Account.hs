module Handler.Account where

import Import
import Permissions
import TransactionQueries
import Data.List
import Data.Fixed
import Data.Maybe
import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.Text as T
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

getAccountUsers :: AccountId -> Handler [(UserId, Text)]
getAccountUsers aid = do 
  blah <- runDB $ E.select $ E.from (\user -> do 
    E.where_ $ E.in_ (user ^. UserId) 
      (E.subList_select $ E.from (\useracct -> do 
        E.where_ $ useracct ^. UserAccountAccount E.==. (E.val aid)
        return $ useracct ^. UserAccountUser))
    return $ (user ^. UserId, user ^. UserIdent)) 
  return $ (\(E.Value a, E.Value b) -> (a,b)) <$> blah

getAccountClubs :: AccountId -> Handler [(ClubId, Text)]
getAccountClubs aid = do 
  blah <- runDB $ E.select $ E.from (\club -> do 
    E.where_ $ E.in_ (club ^. ClubId) 
      (E.subList_select $ E.from (\clubacct -> do 
        E.where_ $ clubacct ^. ClubAccountAccount E.==. (E.val aid)
        return $ clubacct ^. ClubAccountClub))
    return $ (club ^. ClubId, club ^. ClubName)) 
  return $ (\(E.Value a, E.Value b) -> (a,b)) <$> blah

getAccountProjects :: AccountId -> Handler [(ProjectId, Text)]
getAccountProjects aid = do 
  blah <- runDB $ E.select $ E.from (\project -> do 
    E.where_ $ E.in_ (project ^. ProjectId) 
      (E.subList_select $ E.from (\projectacct -> do 
        E.where_ $ projectacct ^. ProjectAccountAccount E.==. (E.val aid)
        return $ projectacct ^. ProjectAccountProject))
    return $ (project ^. ProjectId, project ^. ProjectName)) 
  return $ (\(E.Value a, E.Value b) -> (a,b)) <$> blah

getAccountEmails :: AccountId -> Handler [(EmailId, Text)]
getAccountEmails aid = do
  blah <- runDB $ E.select $ E.from (\email -> do 
    E.where_ $ E.in_ (email ^. EmailId) 
      (E.subList_select $ E.from (\acctemail -> do 
        E.where_ $ acctemail ^. AccountEmailAccount E.==. (E.val aid)
        return $ acctemail ^. AccountEmailEmail))
    return $ (email ^. EmailId, email ^. EmailEmail))
  return $ (\(E.Value a, E.Value b) -> (a,b)) <$> blah

getAccountR :: AccountId -> Handler Html
getAccountR aid = do
  logid <- requireAuthId
  admin <- isAdmin logid
  -- case admin || (logid == uid) of 
  case admin of 
    False -> error "unauthorized"
    True -> do
      mbacct <- runDB $ get aid
      -- mbdacct <- getDuesAccount uid
      users <- getAccountUsers aid
      clubs <- getAccountClubs aid
      projs <- getAccountProjects aid
      emails <- getAccountEmails aid
      ppyls <- getAccountPaypals aid
      pis <- getAccountPaypalInternal aid
      internals <- getAccountInternals aid
      defaultLayout $ do
        [whamlet| 
          <h3> account "#{ fromMaybe "" (accountName <$> mbacct) }"
          <table class="low">
            <tr>
              <th> account owners
            $forall (id, txt) <- users 
              <tr>
                <td> <a href=@{ UserR id }> #{ txt } 
                <td> user
            $forall (id, txt) <- clubs 
              <tr>
                <td> <a href=@{ ClubR id }> #{ txt } 
                <td> club
            $forall (id, txt) <- projs 
              <tr>
                <td> <a href=@{ ProjectR id }> #{ txt } 
                <td> project
          <table class="low">
            <tr>
              <th> account emails
           $forall (id, eml) <- emails 
              <tr>
                <td> #{ eml } 
          <a href=@{AccountDuesR aid} > generate dues transactions
          <h3> internal->internal transactions 
          <a href=@{ AddInternalR aid }> add new 
          <table class="low">
            <tr>
              <th> from account
              <th> to account 
              <th> creator
              <th> date
              <th> amount
              <th> manual
            $forall (id, fromaccount, fromAccountName, mbFromAccountOwners, toaccount, toAccountName, mbToAccountOwners, creatorId, creatorIdent, datetime, amount, manual) <- internals
              <tr>
                <td> 
                  <a href=@{AccountR fromaccount}> #{ fromAccountName }
                <td> 
                  <a href=@{AccountR toaccount}> #{ toAccountName } 
                <td> 
                  <a href=@{UserR creatorId}> #{ creatorIdent }
                <td> #{show datetime}
                <td> #{show amount} 
                <td> #{show manual} 
              <tr>
                <td> #{ fromMaybe "" mbFromAccountOwners } 
                <td> #{ fromMaybe "" mbToAccountOwners } 
          <h3> paypal->internal transactions
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
