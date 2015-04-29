module Handler.AccountDues where

import Import hiding ((==.), (!=.), (||.), delete)
import Database.Esqueleto
import qualified Database.Persist as P
import DuesTransactions
import TransactionQueries 
import Data.Fixed
import qualified Data.Text as T
import Control.Monad
--import qualified Data.Maybe as M
import qualified Data.List as L
import Data.Time.Clock
-- import Data.Time.Calendar
import Util

toAcctTrans :: (PaypalId, AccountId, Text, UserId, Text, UTCTime, Centi, Bool) -> AcctTrans 
toAcctTrans (_,_,_,_,_,time,amt,_) = AcctTrans time amt False

data Meh = Meh 
  {
  mehVal :: Text
  }

mehForm :: Form Meh
mehForm = renderDivs $ Meh 
  <$> areq hiddenField "" (Just (T.pack "a"))

getAccountDuesR :: AccountId -> Handler Html
getAccountDuesR aid = do 
  mbaccount <- runDB $ get aid
  acct <- unMaybeMsg mbaccount "account not found"
  drs <- getDuesRates (accountClub acct) 
  ppi <- getAccountPaypalInternal aid
  mbclub <- runDB $ get (accountClub acct)
  clubduesacct <- unMaybeMsg (join (clubDuesaccount <$> mbclub)) 
                             "club dues account not found"
  allint <- getAccountInternalsFromTo aid clubduesacct
  -- calculate dues transactions.
  --let intdues = (\(a,b,c) -> AcctTrans a b c) <$> filter (\(_,_,manual) -> manual) allint 
  let inttrans = (\(a,b,c) -> AcctTrans a b c) <$> allint 
      alltrans = L.sortBy (\a b -> compare (atDate a) (atDate b)) 
                         (inttrans ++ (toAcctTrans <$> ppi))
      dues = calcDues alltrans drs
  let merged = L.sortBy sortftn ((Left <$> nondues) ++ (Right <$> dues))
      nondues = filter (\(AcctTrans _ _ fordues) -> not fordues) alltrans
      sortftn a b = compare (edate a) (edate b) 
      edate (Left (AcctTrans dt _ _)) = dt :: UTCTime
      edate (Right (DuesEntry dt _ _ _)) = dt :: UTCTime 
  (okform,enc) <- generateFormPost mehForm
  defaultLayout $ do [whamlet|
    <br> #{ show allint }
    <br> #{ show alltrans }
    <br> account dues
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
        <th> manual
      $forall a <- merged
        $case a
          $of Right (DuesEntry date amount balance manual)
            <tr>
              <td> #{ show date } 
              <td> 
              <td> #{ show amount } 
              <td> #{ show balance } 
              <td> #{ show manual } 
          $of Left (AcctTrans date amount _)
            <tr>
              <td> #{ show date }
              <td> #{ show amount } 
              <td>
              <td>
    <form method=post enctype=#{ enc }>
      ^{ okform }
      <input type=submit value="update">
    |]

postAccountDuesR :: AccountId -> Handler Html
postAccountDuesR aid = do 
  logid <- requireAuthId
  -- admin <- isAdmin logid
  mbacct <- runDB $ P.get aid
  acct <- unMaybeMsg mbacct "account not found" 
  mbclub <- runDB $ P.get (accountClub acct)
  club <- unMaybeMsg mbclub "club not found"
  let mbduesacct = clubDuesaccount club
  duesacct <- unMaybeMsg mbduesacct "club dues account not found!"
  ((res,_),_) <- runFormPost mehForm
  case res of 
    FormSuccess meh -> do 
      -- dump trannies into the internal table.
      -- first delete all the auto internal transactions.
      _ <- runDB $ delete $ from (\internal -> do 
          where_ $ (internal ^. InternalFromaccount ==. (val aid))
            &&. (internal ^. InternalManual ==. (val False))
            &&. (internal ^. InternalToaccount ==. (val duesacct)))
      -- create dues recs. 
      drs <- getDuesRates (accountClub acct) 
      ppi <- getAccountPaypalInternal aid
      let dues = calcDues (toAcctTrans <$> ppi) drs
          internals = (makeInternal logid aid duesacct) <$> dues
      -- create internals from the dues recs.
      mapM (\internal -> runDB $ insert internal) internals
      redirect $ AccountR aid
    _ -> error "form error"

makeInternal :: UserId -> AccountId -> AccountId -> DuesEntry -> Internal
makeInternal creator fromacct toacct (DuesEntry date amount balance manual) = 
  Internal { 
    internalFromaccount = fromacct,
    internalToaccount = toacct,
    internalCreator = creator,
    internalDate = date,
    internalAmount = amount,
    internalManual = manual } 


