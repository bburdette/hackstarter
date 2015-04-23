module Handler.AccountDues where

import Import
import DuesTransactions
import TransactionQueries 
import Data.Fixed
-- import Data.Maybe
import Data.List
import Data.Time.Clock
-- import Data.Time.Calendar
import Util

toEzTrans :: (PaypalId, AccountId, Text, UserId, Text, UTCTime, Centi, Bool) -> (UTCTime, Centi)
toEzTrans (_,_,_,_,_,time,amt,_) = (time,amt) 

getAccountDuesR :: AccountId -> Handler Html
getAccountDuesR aid = do 
  mbaccount <- runDB $ get aid
  acct <- unMaybeMsg mbaccount "account not found"
  drs <- getDuesRates (accountClub acct) 
  ppi <- getAccountPaypalInternal aid
  let dues = calcDues (toEzTrans <$> ppi) drs
      merged = sortBy sortftn ((Left <$> ppi) ++ (Right <$> dues))
      sortftn a b = compare (edate a) (edate b) 
      edate (Left (_,_,_,_,_,dt,_,_)) = dt
      edate (Right (DuesEntry dt _ _)) = dt 
  defaultLayout $ do [whamlet|
    account dues
    <table> 
      <tr>
        <th> 
        <th> internal 
        <th> dues 
        <th> 
       <tr>
        <th> date
        <th> amount
        <th> amount
        <th> balance
      $forall a <- merged
        $case a
          $of Right (DuesEntry date amount balance)
            <tr>
              <td> #{ show date } 
              <td> 
              <td> #{ show amount } 
              <td> #{ show balance } 
          $of Left (_,_,_,_,_,date,amount,_)
            <tr>
              <td> #{ show date }
              <td> #{ show amount } 
              <td>
              <td>
    |]

postAccountDuesR :: AccountId -> Handler Html
postAccountDuesR = error "Not yet implemented: postAccountDuesR"
