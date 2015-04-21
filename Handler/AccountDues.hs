module Handler.AccountDues where

import Import
import DuesTransactions
import TransactionQueries 
import Data.Fixed
-- import Data.Maybe
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
  defaultLayout $ do [whamlet|
    account dues
    <table> 
      <tr>
        <th> date
        <th> amount
        <th> balance
      $forall (DuesEntry date amount balance) <- dues
        <tr>
          <td> #{ show date }
          <td> #{ show amount } 
          <td> #{ show balance } 
    |]

postAccountDuesR :: AccountId -> Handler Html
postAccountDuesR = error "Not yet implemented: postAccountDuesR"
