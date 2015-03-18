module Handler.Utilities where

import Import
import System.IO
import Data.Conduit
import qualified Data.Text as T
import System.Directory
import Data.Time.Clock
import qualified Data.Time.Format as F
import Text.CSV
import qualified Data.Map as M
import qualified Data.List as L
import Data.Char
import Data.Fixed
import Data.Time.Calendar
import System.Locale
import Permissions
import qualified Data.Maybe as MB
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

paypalDir = "paypals"
bankDir = "banks"

paypalForm :: Form FileInfo
paypalForm = renderDivs $ 
    fileAFormReq "" 

data Meh = Meh 
  {
  mehVal :: Text
  }

mehForm :: Form Meh
mehForm = renderDivs $ Meh 
  <$> areq hiddenField "" (Just (T.pack "a"))

data Blah = Blah
  {
    chegBockses :: [Int]
  }

blahForme :: [(Text,Int)] -> Form Blah
blahForme choisez = renderDivs $ Blah
  <$> areq (checkboxesFieldList choisez) "yeah" Nothing 

parsePaypal :: FilePath -> IO [(M.Map String String)]
parsePaypal fp = do
  meh <- parseCSVFromFile fp 
  case meh of 
    Right csvl -> 
      case 2 > (length csvl) of
        True -> return []
        False -> do 
          let heads = fmap (dropWhile isSpace) $ L.head csvl
              zipmap heads datas = M.fromList $ zip heads datas
          return $ fmap (zipmap heads) (L.tail csvl)
    Left err -> return []

ppToTransaction :: (M.Map String String) -> Maybe PaypalTransaction
ppToTransaction mp = do
  date <- M.lookup "Date" mp 
  time <- M.lookup "Time" mp
  zone <- M.lookup "Time Zone" mp
  datetime <- F.parseTime defaultTimeLocale "%-m/%-d/%Y %H:%M:%S %Z" $
    date ++ " " ++ time ++ " " ++ zone
  name <- fmap T.pack $ M.lookup "Name" mp 
  ppType <- fmap T.pack $ M.lookup "Type" mp 
  title <- fmap T.pack $ M.lookup "Item Title" mp 
  fgamount <- fmap (\x -> read (filter ((/=) ',') x)) $ M.lookup "Gross" mp :: Maybe Float
  fnamount <- fmap (\x -> read (filter ((/=) ',') x)) $ M.lookup "Net" mp :: Maybe Float
  let gamount = MkFixed (truncate $ fgamount * 100.0)
      namount = MkFixed (truncate $ fnamount * 100.0)
  fromEmail <- fmap T.pack $ M.lookup "From Email Address" mp
  toEmail <- fmap T.pack $ M.lookup "To Email Address" mp
  transactionId <- fmap T.pack $ M.lookup "Transaction ID" mp
  Just $ PaypalTransaction datetime name ppType title gamount namount fromEmail toEmail transactionId

-- eppToTransaction :: (M.Map String String) -> Maybe UTCTime 
eppToTransaction mp = ( 
  M.lookup "Date" mp 
  , M.lookup "Time" mp
  , M.lookup "Time Zone" mp
--  , F.parseTime defaultTimeLocale "%-m/%-d/%Y %H:%M:%S %Z" $
--    date ++ " " ++ time ++ " " ++ zone
  , fmap T.pack $ M.lookup "Name" mp 
  , fmap (\x -> read (filter ((/=) ',') x)) $ M.lookup "Net" mp :: Maybe Float
--  let amount = truncate $ famount * 100.0
  , fmap T.pack $ M.lookup "From Email Address" mp
  , fmap T.pack $ M.lookup "Transaction ID" mp )

{-
  name <- fmap T.pack $ M.lookup "Name" mp 
  famount <- fmap read $ M.lookup "Net" mp :: Maybe Float
  let amount = truncate $ famount * 100.0
  email <- fmap T.pack $ M.lookup "From Email Address" mp
  transactionId <- fmap T.pack $ M.lookup "Transaction ID" mp
  Just $ Transaction datetime name amount email transactionId
-}

data PaypalTransaction = PaypalTransaction 
  { dateTime :: UTCTime
  , name :: Text
  , ppType :: Text
  , title :: Text
  , amountGross :: Centi
  , amountNet :: Centi
  , fromEmail :: Text
  , toEmail :: Text
  , transactionId :: Text
  }
  deriving Show

-- add paypal transaction, creating emails if necessary.
addPaypalTransaction :: UserId -> PaypalTransaction -> Handler (Maybe (Key Paypal))
addPaypalTransaction creator trans = do 
  mbFromEmail <- runDB $ getBy $ UniqueEmail (fromEmail trans)
  mbToEmail <- runDB $ getBy $ UniqueEmail (toEmail trans)
  Entity ekey eml <- case mbFromEmail of 
    Nothing -> do
      now <- lift getCurrentTime
      let eml = Email (fromEmail trans) Nothing Nothing
      key <- runDB $ insert eml
      return (Entity key eml) 
    Just eml -> return eml
      -- if there's not a user record for the email, create one now.
  Entity tekey teml <- case mbToEmail of 
    -- add email records for 'to' emails, but not user accounts.
    Nothing -> do
      now <- lift getCurrentTime
      let eml = Email (toEmail trans) Nothing Nothing
      key <- runDB $ insert eml
      return (Entity key eml) 
    Just eml -> return eml
  runDB $ insertUnique $ 
    Paypal (transactionId trans)
           (name trans)
           (ppType trans)
           (title trans)
           (Just ekey)
           (Just tekey)
           (amountGross trans)
           (amountNet trans)
           creator
           (dateTime trans)

data BankTransaction = BankTransaction 
  { bTransactionId :: Text
  , bDate :: Day 
  , bDescription :: Text
  , bMemo :: Text
  , bAmount :: Centi
  , bCheckNumber :: Maybe Int 
  }
  deriving Show

parseBank :: FilePath -> IO [(M.Map String String)]
parseBank fp = do
  fstr <- readFile fp 
  let parze = breakOn "Transaction Number" fstr
      meh = fmap (parseCSV fp) parze
  case meh of 
    Just (Right csvl) -> 
      case 2 > (length csvl) of
        True -> return []
        False -> do 
          let heads = fmap (dropWhile isSpace) $ L.head csvl
              zipmap heads datas = M.fromList $ zip heads datas
          return $ fmap (zipmap heads) (L.tail csvl)
    Just (Left err) -> return []
    Nothing -> return []

mebbe :: (Read a) => Maybe [Char] -> Maybe a
mebbe Nothing = Nothing
mebbe (Just t) = 
  if t == "" 
    then Nothing
    else Just $ read t

matches :: Eq a => [a] -> [a] -> Bool
matches [] _ = True
matches (x:xs) (y:ys) = 
  (x == y) && matches xs ys
matches _ [] = False

breakOn :: Eq a => [a] -> [a] -> Maybe [a]
breakOn [] blah = Just blah
breakOn match [] = Nothing
breakOn match (x:xs) = 
  if matches match (x:xs)
    then Just (x:xs)
    else breakOn match xs

bkToTransaction :: (M.Map String String) -> Maybe BankTransaction
bkToTransaction mp = do
  transactionId <- fmap T.pack $ M.lookup "Transaction Number" mp
  date <- M.lookup "Date" mp 
  description <- fmap T.pack $ M.lookup "Description" mp
  memo <- fmap T.pack $ M.lookup "Memo" mp
  day <- F.parseTime defaultTimeLocale "%-m/%-d/%Y" date
  let deb = mebbe $ M.lookup "Amount Debit" mp :: Maybe Centi
      cred = mebbe $ M.lookup "Amount Credit" mp :: Maybe Centi
      amount = maybe (maybe (MkFixed 0) id cred) id deb
  let check = mebbe (M.lookup "Check Number" mp) :: Maybe Int
  Just $ BankTransaction transactionId day description memo amount check

-- add bank transaction, creating emails if necessary.
addBankTransaction :: UserId -> BankTransaction -> Handler (Maybe (Key Bank))
addBankTransaction creator trans = do 
 runDB $ insertUnique $ 
    Bank (bTransactionId trans)
         (bDate trans)
         (bDescription trans)
         (bMemo trans)
         (bAmount trans)
         (bCheckNumber trans)

-- version where we create user accounts if they don't exist.
-- uh oh, user idents are sposed to be unique!!
{-
addTransactionWUser :: UserId -> DuesRateId -> PaypalTransaction -> Handler (Maybe (Key Paypal))
addTransactionWUser creator defaultdr trans = do 
{-
  mbemail <- runDB $ getBy $ UniqueEmail (email trans)
  Entity ekey eml <- case mbemail of 
            Nothing -> do
              now <- lift getCurrentTime
              ukey <- runDB $ insert $ User (email trans) (name trans) Nothing defaultdr 0 (utctDay now)
              let eml = Email (email trans) (Just ukey) Nothing
              key <- runDB $ insert eml
              return (Entity key eml) 
            Just eml -> return eml
-}
  mbFromEmail <- runDB $ getBy $ UniqueEmail (fromEmail trans)
  mbToEmail <- runDB $ getBy $ UniqueEmail (toEmail trans)
  Entity ekey eml <- case mbFromEmail of 
    Nothing -> do
      now <- lift getCurrentTime
      -- use email as user ident
      ukey <- runDB $ insert $ User (fromEmail trans) (name trans) Nothing defaultdr 0 (utctDay now)
      let eml = Email (fromEmail trans) (Just ukey) Nothing
      key <- runDB $ insert eml
      return (Entity key eml) 
    Just (Entity ekey eml) -> 
      -- if there's not a user record for the email, create one now.
      case (emailUser eml) of 
        Just _ -> return $ Entity ekey eml
        Nothing -> do
          -- use email as user ident
          now <- lift getCurrentTime
          ukey <- runDB $ insert $ User (fromEmail trans) (name trans) Nothing defaultdr 0 (utctDay now)
          let eml2 = eml { emailUser = Just ukey } 
          runDB $ replace ekey eml2 
          return (Entity ekey eml2) 
  Entity tekey teml <- case mbToEmail of 
    -- add email records for 'to' emails, but not user accounts.
    Nothing -> do
      now <- lift getCurrentTime
      let eml = Email (toEmail trans) Nothing Nothing
      key <- runDB $ insert eml
      return (Entity key eml) 
    Just eml -> return eml
  runDB $ insertUnique $ 
    Paypal (Just (transactionId trans))
           (emailUser eml)
           (Just ekey)
           (emailUser teml)
           (Just tekey)
           (amountGross trans)
           (amountNet trans)
           creator
           False
           (dateTime trans)
           Nothing
-}

{-
-- version where we don't create user accounts.
addTransaction :: UserId -> Transaction -> Handler (Maybe (Key Paypal))
addTransaction creator trans = do 
  mbemail <- runDB $ getBy $ UniqueEmail (email trans)
  Entity ekey eml <- case mbemail of 
            Nothing -> do
              let eml = Email (email trans) Nothing Nothing
              key <- runDB $ insert eml
              return (Entity key eml) 
            Just eml -> return eml
  runDB $ insertUnique $ 
    Paypal (Just (transactionId trans))
           (emailUser eml)
           (Just ekey)
           (amountGross trans)
           (amountNet trans)
           creator
           False
           (dateTime trans)
           Nothing
-}

{-

  create missing users:
    for emails in paypal transactions that have no existing accounts, 
    create user records.  Use the Name field for name, etc.  


createMissingUsers :: Handler [UserId]
createMissingUsers = do 
  -- emails that are in paypal but have no accounts.
  res <- runDB $ E.select $ E.from $ 
    (\(E.LeftOuterJoin (E.InnerJoin paypal email) accountemail)  -> do
      E.where_ accountemail ^. AccountEmailEmail ^.==. email ^. EmailId 
      E.where_ email ^. EmailId ^.==. paypal ^. PaypalFromemail
      E.where_ isNothing (accountemail ^. AccountEmailEmail)
      return ( paypal ^. PaypalName, email ^. EmailEmail ) 
       
                res <- runDB $ E.select $ E.from $ 
                  (\(E.LeftOuterJoin (E.LeftOuterJoin paypal email) accountemail)  -> do
                    E.where_ $ (paypal ^. PaypalFromemail E.==. email E.?. EmailId) 
                    E.where_ $ accountemail E.?. AccountEmailEmail E.==. email E.?. EmailId 
                    E.where_ $ E.isNothing (accountemail E.?. AccountEmailEmail)
                    return ( paypal ^. PaypalName, email E.?. EmailEmail )) 
 -}

unMaybe :: Maybe a -> Handler a
unMaybe mba = 
  case mba of 
    Just mba -> return mba
    Nothing -> error "nothing"

getUtilitiesR :: Handler Html
getUtilitiesR = do
  logid <- requireAuthId
  requireAdmin logid 
  let choices = [("one", 1), ("two", 2), ("three", 3)]
  (blahWidget, blahenc) <- generateFormPost $ identifyForm "blah" $ blahForme choices
  (cppuFormWidget, cppuFormEnctype) <- generateFormPost $ identifyForm "cppu" mehForm
  (cpptFormWidget, cpptFormEnctype) <- generateFormPost $ identifyForm "cppt" mehForm
  (ppFormWidget, ppFormEnctype) <- generateFormPost $ identifyForm "paypal" $ renderDivs $
    fileAFormReq "Upload (UTF-8) paypal transaction file:"
  (bkFormWidget, bkFormEnctype) <- generateFormPost $ identifyForm "bank" $ renderDivs $
    fileAFormReq "Upload (UTF-8) bank transaction file:"
  let submission = Nothing :: Maybe (FileInfo, Text)
  defaultLayout $ do
      aDomId <- newIdent
      setTitle "admin utilities"
      [whamlet|
        <form method=post enctype=#{ppFormEnctype}>
          ^{blahWidget}
          <input type=submit value="blah">
        <form method=post enctype=#{ppFormEnctype}>
          ^{ppFormWidget}
          <input type=submit value="upload">
        <form method=post> 
          ^{cppuFormWidget}
          <input type=submit value="create paypal users">
        <form method=post> 
          ^{cpptFormWidget}
          <input type=submit value="create paypal transactions">
        <form method=post enctype=#{bkFormEnctype}>
          ^{bkFormWidget}
          <input type=submit value="upload">
        In case of error: convert to UTF8 using vim like so:
        <br> :set fileencoding=utf8
        <br> :w <filename>
      |]

postUtilitiesR :: Handler Html
postUtilitiesR = do
  logid <- requireAuthId
  requireAdmin logid
  mbdrid <- checkDuesRate "default" 0
  drid <- unMaybe mbdrid
  ((cppuresult, _), _) <- runFormPost $ identifyForm "cppu" mehForm 
  ((cpptresult, _), _) <- runFormPost $ identifyForm "cppt" mehForm
  ((ppresult, ppFormWidget), ppFormEnctype) <- runFormPost $ identifyForm "paypal" paypalForm 
  ((bkresult, ppFormWidget), bkFormEnctype) <- runFormPost $ identifyForm "bank" paypalForm 
  let handlerName = "postUtilitiesR" :: Text
    in do
      case ppresult of
        FormFailure meh -> error $ show meh
        FormSuccess fi -> do
          lift $ createDirectoryIfMissing True paypalDir
          now <- lift $ getCurrentTime
          let fname = paypalDir ++ "//paypal" ++ (show now)
          lift $ fileMove fi fname
          recs <- lift $ parsePaypal fname
          let transes = (MB.catMaybes (map ppToTransaction recs))
          keys <- mapM (addPaypalTransaction logid) transes 
          let lrecs = length recs
              ltranses = length transes
              lkeys = length (MB.catMaybes keys)
          defaultLayout $ do
            aDomId <- newIdent
            setTitle "Yeeaaaahhh!"
            [whamlet|
              <br> Paypal transactions imported!
              <br> #{show lrecs} records found in file.
              <br> #{show ltranses} records being valid-looking transactions.
              <br> we wrote #{show lkeys} transaction records.  
              <br> records with transaction IDs already in the database are skipped.
            |]
        FormMissing -> 
          case bkresult of
            FormFailure meh -> error $ show meh
            FormSuccess fi -> do
              lift $ createDirectoryIfMissing True bankDir
              now <- lift $ getCurrentTime
              let fname = bankDir ++ "//bank" ++ (show now)
              lift $ fileMove fi fname
              recs <- lift $ parseBank fname
              let transes = (MB.catMaybes (map bkToTransaction recs))
              keys <- mapM (addBankTransaction logid) transes 
              let lrecs = length recs
                  ltranses = length transes
                  lkeys = length (MB.catMaybes keys)
              defaultLayout $ do
                aDomId <- newIdent
                setTitle "Yeeaaaahhh!"
                [whamlet|
                  <br> Bank transactions imported!
                  <br> #{show lrecs} records found in file.
                  <br> #{show ltranses} records being valid-looking transactions.
                  <br> we wrote #{show lkeys} transaction records.  
                  <br> records with transaction IDs already in the database are skipped.
                |]
            FormMissing -> case cppuresult of 
              FormSuccess meh -> do 
                restoo <- runDB $ E.select $ E.from $ 
                  (\(E.LeftOuterJoin paypal email) -> do
                    E.where_ $ paypal ^. PaypalFromemail E.==. email E.?. EmailId
                    E.where_ $ E.notExists $ 
                      E.from $ \accountemail -> do
                        E.where_ (paypal ^. PaypalFromemail E.==. 
                                  accountemail E.?. AccountEmailEmail)
                    return (paypal ^. PaypalName, paypal ^. PaypalId, email E.?. EmailEmail)) 
                defaultLayout $ do [whamlet|
                  cppuresult:
                  <table>
                    <tr>
                      <th> ppn
                      <th> pid
                      <th> email
                    $forall (E.Value ppn, E.Value pid, eml) <- restoo
                      <tr>
                        <td> #{ ppn }
                        <td> #{ show pid }
                        $case eml 
                          $of (E.Value (Just email))
                            <td> #{ email } 
                          $of (E.Value Nothing)
                            <td> 
                 |]
              FormFailure err -> error $ show err
              FormMissing -> case cpptresult of 
                FormSuccess meh -> do 
                  defaultLayout $ do [whamlet|
                    cpptresult:
                    <br> #{ (mehVal meh) }
                  |]
                FormFailure err -> error $ show err
                FormMissing -> error "form missing"             

{-           

    for emails in paypal transactions that have no existing accounts, 
    create user records.  Use the Name field for name, etc.  

  select email, name from paypal where email not in (select email from account_email)

  select * from email where id in (select email from account_email);

-}
 

