module Handler.PaypalDetail where

import Import
import Permissions
import TransactionQueries
import Util
import Data.Maybe

getPaypalDetailR :: PaypalId -> Handler Html
getPaypalDetailR pid = do 
  logid <- requireAuthId
  requireAdmin logid
  mbpp <- getPaypal pid
  (date, amtg, amtn, name, desc, memo, from, to, creatorid, creatorident) <- unMaybeMsg mbpp "paypal not found"
  pis <- getPaypalPaypalInternal pid 
  defaultLayout $ do [whamlet|
    <table class="low">
      <tr> 
        <th> desc
        <th> value
      <tr>
        <td> Date
        <td> #{ show date } 
      <tr>
        <td> Gross
        <td> #{ show amtg } 
      <tr>
        <td> Net
        <td> #{ show amtn } 
      <tr>
        <td> Name
        <td> #{ name } 
      <tr>
        <td> Description
        <td> #{ desc } 
      <tr>
        <td> Memo
        <td> #{ memo } 
      <tr>
        <td> From email
        <td> #{ fromMaybe "" from } 
      <tr>
        <td> To email
        <td> #{ fromMaybe "" to } 
      <tr>
        <td> creator
        <td> #{ creatorident } 
    <br> paypal->internal from/to this transaction 
    <table class="low">
      <tr>
        <th> datetime 
        <th> amount 
        <th> to account
        <th> created by
        <th> manual?
      $forall (ppid, toacctid, toacctname, creatorid, creatorident, datetime, amount, manual) <- pis
        <tr>
          <td> #{ show datetime }
          <td> #{show amount} 
          <td> <a href=@{AccountR toacctid} > #{ toacctname }
          <td> 
            <a href=@{UserR creatorid}> #{ creatorident }
          $if manual  
            <td> #{show manual} 
          $else
            <td>
      |]

postPaypalDetailR :: PaypalId -> Handler Html
postPaypalDetailR = error "Not yet implemented: postPaypalDetailR"
