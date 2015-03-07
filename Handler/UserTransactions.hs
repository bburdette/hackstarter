module Handler.UserTransactions where

import Import
import Permissions
import Data.List
import Data.Time.Clock
import Data.Time.Calendar
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

{-
getUserBalance :: UserId -> Handler Int
getUserBalance uid = do
  (runDB $ E.select 
    $ E.from $ \lolwut -> do 
      let sumamt = (E.sum_ (lolwut ^. LedgerAmountGross))
      E.where_ $ lolwut ^. LedgerFromuser E.==. (E.val $ Just uid)
      return sumamt) >>= (\x -> 
        case x of 
          [E.Value (Just amt)] -> return amt
          _ -> return 0) 
-}

data DuesEntry = DuesEntry {
  date :: UTCTime,
  amount :: Int,
  balance :: Int
  }
  deriving Show

-- if we allowed paying any amount for dues, carrying over a balance and so forth, how would we determine the
-- dues rates without programming them in manually?
-- could assume 'regular' unless there is a dues amount.
-- if there is a dues amount, that's the rate until there is another valid dues amount.
-- ok lets do it that way.

-- assuming transactions are in ascending order by time.
-- assuming all these are FOR dues
calcDues :: [(UTCTime, Int)] -> [Int] -> [DuesEntry]
calcDues transactions duesrates = 
  makeDues (filter ((<) 0) (sort duesrates)) Nothing 0 transactions

makeDues :: [Int] -> (Maybe (UTCTime, Int)) -> Int -> [(UTCTime, Int)] -> [DuesEntry]
-- if no more transactions, we're done making dues entries.
makeDues _ Nothing _ [] = []
makeDues drs (Just (time,amt)) bal [] = 
  if (bal >= amt)
    then let nexttime = addMonths time 1 in 
      (DuesEntry nexttime amt bal) : 
        (makeDues drs (Just (nexttime, amt)) (bal-amt) [])
    else []
-- add transactions until the balance is >= one of the dues rates.  that's our initial
-- dues rate, and dues transaction datetime.
makeDues duesrates Nothing argbalance ((time,amt):rest) = 
  let balance = argbalance + amt
      rates = takeWhile ((>=) balance) duesrates in 
  case rates of 
    [] -> makeDues duesrates Nothing (balance + amt) rest
    ratez -> let rate = last ratez in 
      (DuesEntry time rate balance) : 
         makeDues duesrates (Just (time, rate)) (balance - rate) rest
makeDues duesrates (Just (lasttime, lastrate)) balance ((time,amt):rest) = 
  let nextdate = addMonths lasttime 2 in
  if (time > nextdate) 
    then makeDues duesrates (Just ((addMonths lasttime 1), lastrate)) balance ((time,amt):rest)
    else let
      newbalance = balance + amt
      rate = if (elem amt duesrates) then amt else lastrate
      in
      if newbalance >= rate
        then let ddate = addMonths lasttime 1 
                 nbal = newbalance - rate in 
          (DuesEntry ddate rate newbalance) : makeDues duesrates (Just (ddate, rate)) nbal rest 
        else makeDues duesrates (Just (lasttime, rate)) newbalance rest 

addMonths :: UTCTime -> Integer -> UTCTime
addMonths start months =
  start { utctDay = addGregorianMonthsClip months (utctDay start) } 

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


getUserTransactionsR :: UserId -> Handler Html
getUserTransactionsR uid = error "unimplelmented" 
{-
getUserTransactionsR uid = do
  logid <- requireAuthId
  admin <- isAdmin logid
  case admin || (logid == uid) of 
    False -> error "unauthorized"
    True -> do
      mbusr <- runDB $ get uid
      bal <- getUserBalance uid
      case mbusr of 
        Nothing -> error "invalid user id"
        Just usr -> do 
          ledges <- runDB $ E.select 
            $ E.from $ \(E.InnerJoin user ledger) -> do 
              E.where_ $ ledger ^. LedgerFromuser E.==. (E.val $ Just uid)
              E.on $ user ^. UserId E.==. ledger ^. LedgerCreator
              E.orderBy $ [E.asc ( ledger ^. LedgerDate)]
              return 
                ( ledger ^. LedgerAmountGross,
                  ledger ^. LedgerAmountNet,
                  ledger ^. LedgerDate,
                  ledger ^. LedgerCreator,
                  user ^. UserIdent ) 
          plainledges <- runDB $ selectList [LedgerFromuser ==. Just uid] [Asc LedgerDate]
          duesrates <- runDB $ selectList [] [Asc DuesRateAmount]
          let transes = map (\(Entity xk x) -> (ledgerDate x, ledgerAmountGross x)) 
                            plainledges
              drs = map (\(Entity k v) -> duesRateAmount v) duesrates
              dues = calcDues transes drs
          -- let dues = take 10 (calcDues transes drs)
          defaultLayout $ do
            [whamlet| 
              <h3> transactions for user: 
                <a href=@{UserR uid}>#{userIdent usr} 
              <br>
                $if (admin)
                  <a href=@{AddLedgerEntryR uid}> add transaction 
              <table class="sum">
                <tr>
                  <th> balance
                <tr>
                  <td> #{show bal}
              <table class="low">
                <tr>
                  <th> gross
                  <th> net
                  <th> datetime
                  <th> created by
                $forall (E.Value gamount, E.Value namount, E.Value datetime, E.Value creatorid, E.Value creatorIdent) <- ledges
                  <tr>
                    <td> #{gamount}
                    <td> #{namount}
                    <td> #{show datetime}
                    <td> #{creatorIdent}
              <h3> Estimated dues payments
              <table class="dues">
                <tr>
                  <th> datetime
                  <th> amount  
                  <th> balance 
                $forall (DuesEntry date amount bal) <- dues 
                  <tr>
                    <td> #{show date}
                    <td> #{show amount}
                    <td> #{show bal}
            |]
-}
 
postUserTransactionsR :: UserId -> Handler Html
postUserTransactionsR = error "Not yet implemented: postUserTransactionsR"
