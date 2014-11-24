module Handler.UserTransactions where

import Import
import Permissions
import Data.List
import Data.Time.Clock
import Data.Time.Calendar
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

getUserBalance :: UserId -> Handler Int
getUserBalance uid = do
  (runDB $ E.select 
    $ E.from $ \lolwut -> do 
      let sumamt = (E.sum_ (lolwut ^. LedgerAmountGross))
      E.where_ $ lolwut ^. LedgerUser E.==. (E.val $ Just uid)
      return sumamt) >>= (\x -> 
        case x of 
          [E.Value (Just amt)] -> return amt
          _ -> return 0) 

data DuesEntry = DuesEntry {
  date :: UTCTime,
  amount :: Int
  }

-- if we allowed paying any amount for dues, carrying over a balance and so forth, how would we determine the
-- dues rates without programming them in manually?
-- could assume 'regular' unless there is a dues amount.
-- if there is a dues amount, that's the rate until there is another valid dues amount.
-- ok lets do it that way.


calcDues :: UserId -> [Int] -> Handler [DuesEntry]
calcDues uid duesrates = do 
  -- assuming all these are FOR dues, and are an amount in the duesrates table.   
  ledges <- runDB $ selectList [LedgerUser ==. Just uid] [Asc LedgerDate]
  -- let transes = filter (\(dt,amt) -> elem amt duesrates) 
  --                     (map (\(Entity xk x) -> (ledgerDate x, ledgerAmountGross x)) ledges)
  let transes = map (\(Entity xk x) -> (ledgerDate x, ledgerAmountGross x)) ledges
  return $ makaDoose (filter ((<) 0) (sort duesrates)) Nothing 0 transes
  -- return $ makeDues duesrates transes


{-
makeDues :: Int -> [Int] -> [(UTCTime, Int)] -> [DuesEntry]
makeDues rate duesrates ((time, amt):xs) = makaDoose rate duesrates time amt xs 
makeDues _ _ [] = []
-}

makaDoose :: [Int] -> (Maybe (UTCTime, Int)) -> Int -> [(UTCTime, Int)] -> [DuesEntry]
-- if no more transactions, we're done making dues entries.
makaDoose _ _ _ [] = []
-- add transactions until the balance is >= one of the dues rates.  that's our initial
-- dues rate, and dues transaction datetime.
makaDoose duesrates Nothing balance ((time,amt):rest) = 
  let rates = takeWhile ((>=) balance) duesrates in 
  case rates of 
    [] -> makaDoose duesrates Nothing (balance + amt) rest
    ratez -> let rate = last ratez in 
      (DuesEntry time rate) : makaDoose duesrates (Just (time, rate)) (balance - rate) rest

makaDoose duesrates (Just (lasttime, lastrate)) balance dooselist = 
  let nextdate = addMonths lasttime 2
      (paid, future) = splitOn (\(time,_) -> time <= nextdate) dooselist
      newbalance = foldl (+) balance (map (\(_,amt) -> amt) paid)
      in
    if newbalance > lastrate
      then let ddate = addMonths lasttime 1 
               nbal = newbalance - lastrate in 
        (DuesEntry ddate nbal) : makaDoose duesrates (Just (ddate, nbal)) nbal future 
      else makaDoose duesrates (Just (lasttime, lastrate)) newbalance future

addMonths :: UTCTime -> Integer -> UTCTime
addMonths start months =
  start { utctDay = addGregorianMonthsClip months (utctDay start) } 

splitOn :: (a -> Bool) -> [a] -> ([a], [a])
splitOn cond lst = 
  spuliton cond [] lst 

spuliton :: (a -> Bool) -> [a] -> [a] -> ([a],[a])
spuliton _ [] frnt = (reverse frnt, [])
spuliton cond (s:ss) frnt = 
  if (cond s) then
    (reverse frnt, (s:ss))
  else
    spuliton cond ss (s:frnt)


getUserTransactionsR :: UserId -> Handler Html
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
              E.where_ $ ledger ^. LedgerUser E.==. (E.val $ Just uid)
              E.on $ user ^. UserId E.==. ledger ^. LedgerCreator
              E.orderBy $ [E.asc ( ledger ^. LedgerDate)]
              return 
                ( ledger ^. LedgerAmountGross,
                  ledger ^. LedgerAmountNet,
                  ledger ^. LedgerDate,
                  ledger ^. LedgerCreator,
                  user ^. UserIdent ) 
          defaultLayout $ do
            [whamlet| 
              <h3> transactions for user: 
                <a href=@{UserR uid}>#{userIdent usr} 
              <br>
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
             |]

postUserTransactionsR :: UserId -> Handler Html
postUserTransactionsR = error "Not yet implemented: postUserTransactionsR"
