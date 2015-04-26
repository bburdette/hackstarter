module Handler.AccountDues where

import Import hiding ((==.), (!=.), (||.), delete)
import Database.Esqueleto
import qualified Database.Persist as P
import DuesTransactions
import TransactionQueries 
import Data.Fixed
import qualified Data.Text as T
-- import Data.Maybe
import qualified Data.List as L
import Data.Time.Clock
-- import Data.Time.Calendar
import Util

toEzTrans :: (PaypalId, AccountId, Text, UserId, Text, UTCTime, Centi, Bool) -> (UTCTime, Centi)
toEzTrans (_,_,_,_,_,time,amt,_) = (time,amt) 

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
  let dues = calcDues (toEzTrans <$> ppi) drs
      merged = L.sortBy sortftn ((Left <$> ppi) ++ (Right <$> dues))
      sortftn a b = compare (edate a) (edate b) 
      edate (Left (_,_,_,_,_,dt,_,_)) = dt
      edate (Right (DuesEntry dt _ _)) = dt 
  (okform,enc) <- generateFormPost mehForm
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
      let dues = calcDues (toEzTrans <$> ppi) drs
          internals = (makeInternal logid aid duesacct) <$> dues
      -- create internals from the dues recs.
      mapM (\internal -> runDB $ insert internal) internals
      redirect $ AccountR aid
    _ -> error "form error"

makeInternal :: UserId -> AccountId -> AccountId -> DuesEntry -> Internal
makeInternal creator fromacct toacct (DuesEntry date amount balance) = 
  Internal { 
    internalFromaccount = fromacct,
    internalToaccount = toacct,
    internalCreator = creator,
    internalDate = date,
    internalAmount = amount,
    internalManual = False }


