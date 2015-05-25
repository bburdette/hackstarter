module DuesTransactions where

import Import
import Permissions
import TransactionQueries
import Data.List
import Data.Fixed
import Data.Time.Clock
import Data.Time.Calendar
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

data DuesEntry = DuesEntry {
  date :: UTCTime,
  amount :: Centi,
  balance :: Centi,
  manual :: Bool
  }
  deriving Show

-- transactions to/from the user dues account.
-- atForDues=True if transaction is a manual dues transaction.
data AcctTrans = AcctTrans {
  atDate :: UTCTime,
  atAmount :: Centi,
  atForDues :: Bool
  }
  deriving Show

getDuesRates :: ClubId -> Handler [Centi] 
getDuesRates cid = do
  drs <- runDB $ selectList [DuesRateClub ==. cid] []
  return $ duesRateAmount . entityVal <$> drs

-- if we allowed paying any amount for dues, carrying over a balance and so forth, how would we determine the
-- dues rates without programming them in manually?
-- could assume 'regular' unless there is a dues amount.
-- if there is a dues amount, that's the rate until there is another valid dues amount.
-- ok lets do it that way.

-- assuming transactions are in ascending order by time.
-- assuming all these are FOR dues
calcDues :: [AcctTrans] -> [Centi] -> [DuesEntry]
calcDues transactions duesrates = 
  makeDues (filter (0<) $ sort duesrates) Nothing Nothing 0 transactions

-- makeDues duesrates lastmonthduesdate lastduesamount balance transactions -> dues entries.
-- the lastmonthduesdate is cleared after a gap in membership.
makeDues :: [Centi] -> Maybe UTCTime -> Maybe Centi -> Centi -> [AcctTrans] -> [DuesEntry]
-- if no more transactions and no previous month dues transaction, we're done making dues entries.
makeDues _ Nothing _ _ [] = []
makeDues _ (Just _) Nothing _ [] = error "makeDues: unreachable!"  -- should never happen!
-- AcctTrans list is empty - no more transactions.  make more transactions as long as 
-- we have a positive balance.
makeDues drs (Just time) (Just amt) bal [] = 
  case bal >= amt of 
    True -> 
      let nexttime = addMonths time 1 
          newbal = bal - amt in 
      (DuesEntry nexttime amt newbal False) : 
        (makeDues drs (Just nexttime) (Just amt) newbal [])
    False -> []
-- no previous dues transaction.
-- add transactions until the balance is >= one of the dues rates, or there's a manual 
-- dues transaction.  that's our initial dues rate, and dues transaction datetime.
makeDues duesrates Nothing Nothing argbalance (acctTrans:rest) = 
  if atForDues acctTrans
    then
      -- make manual dues transation, adjust balance.
      let amt = - (atAmount acctTrans)
          time = (atDate acctTrans)
          newbal = argbalance - amt in
      (DuesEntry time amt newbal True) : 
        makeDues duesrates (Just time) (Just amt) newbal rest
    else
      -- add to balance, check that bal is >= one of the dues rates; choose the largest rate.
      let amt = (atAmount acctTrans)
          time = (atDate acctTrans)
          balance = argbalance + amt
          rates = takeWhile ((>=) balance) duesrates in 
      case rates of 
        [] -> makeDues duesrates Nothing Nothing (balance + amt) rest
        ratez -> let rate = last ratez 
                     newbal = balance - rate in 
          (DuesEntry time rate newbal False) : 
             makeDues duesrates (Just time) (Just rate) newbal rest
-- gap in membership, but we know the previous dues rate. 
-- add transactions until the balance is >= the previous dues rate, or there's a manual 
-- dues transaction.  that's our initial dues rate, and dues transaction datetime.
makeDues duesrates Nothing (Just lastrate) argbalance (acctTrans:rest) = 
  if (atForDues acctTrans) 
    then
      -- make manual dues transation, adjust balance.
      let amt = - (atAmount acctTrans)
          time = (atDate acctTrans)
          newbal = argbalance - amt in
      (DuesEntry time amt newbal True) : 
        makeDues duesrates (Just time) (Just amt) newbal rest
    else
      -- add to balance, check that bal is >= one of the dues rates; choose the largest rate.
      let amt = atAmount acctTrans
          time = atDate acctTrans
          balance = argbalance + amt in
          -- rates = takeWhile ((>=) balance) duesrates in 
      if (balance < lastrate)
        then 
          makeDues duesrates Nothing (Just lastrate) balance rest
        else
          let newbal = balance - lastrate in 
          (DuesEntry time lastrate newbal False) : 
             makeDues duesrates (Just time) (Just lastrate) newbal rest
-- there is a previous dues transaction.  
makeDues duesrates (Just lasttime) (Just lastrate) balance ((AcctTrans time amt fordues):rest) = 
  if fordues
    then 
      -- make manual dues transation, adjust balance.
      let mamt = - amt   -- manual dues amts are negative! 
          newbal = balance - mamt in  
      (DuesEntry time mamt newbal True) : 
        makeDues duesrates (Just time) (Just mamt) newbal rest
    else
      -- add to the balance as normal.    
      let nextdate = addMonths lasttime 2 in  -- this is how long before dues payment is missed.
      if (time > nextdate)
        then 
          -- skip making a dues transaction this time. its a gap in membership.
          -- pass an updated 'pretend' last dues datetime though, so that if the balance goes positive
          -- they start at the same day/time as before, but different month.
          -- makeDues duesrates (Just ((addMonths lasttime 1), lastrate)) 
          makeDues duesrates Nothing (Just lastrate)
            balance ((AcctTrans time amt fordues):rest)
        else 
          -- we're still within the nextdate time.  update the balance and check whether there's enough 
          -- balance for dues.
          let newbalance = balance + amt
              -- switch to a new dues rate if there's an exact match!
              -- rate = if (elem amt duesrates) then amt else lastrate
              rate = lastrate
          in
          if newbalance >= rate
            then let ddate = addMonths lasttime 1 
                     nbal = newbalance - rate in 
               (DuesEntry ddate rate nbal False) : makeDues duesrates (Just ddate) (Just rate) nbal rest 
            else makeDues duesrates (Just lasttime) (Just rate) newbalance rest 

addMonths :: UTCTime -> Integer -> UTCTime
addMonths start months =
  start { utctDay = addGregorianMonthsClip months (utctDay start) } 

-- (i would have tried to implement this with 'splitAt' and 'findIndex')
splitOn :: (a -> Bool) -> [a] -> ([a], [a])
splitOn cond lst = 
  spuliton cond [] lst

spuliton :: (a -> Bool) -> [a] -> [a] -> ([a],[a])
spuliton _ [] [] = ([], [])
spuliton _ frnt [] = (reverse frnt, [])
spuliton cond frnt (s:ss)  = 
  if (cond s) then
    (reverse frnt, (s:ss))
  else
    spuliton cond (s:frnt) ss



