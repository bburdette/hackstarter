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
import System.Locale
import Permissions
import qualified Data.Maybe as MB

paypalDir = "paypals"

sampleForm :: Form FileInfo
sampleForm = renderDivs $ 
    fileAFormReq "Upload (UTF-8) paypal transaction file:"

{-
sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Upload paypal transaction file:"
    <*> areq textField "Save as: " Nothing
-}

getUtilitiesR :: Handler Html
getUtilitiesR = do
  logid <- requireAuthId
  requireAdmin logid 
  (formWidget, formEnctype) <- generateFormPost sampleForm
  let submission = Nothing :: Maybe (FileInfo, Text)
  defaultLayout $ do
      aDomId <- newIdent
      setTitle "admin utilities"
      [whamlet|
        <form method=post enctype=#{formEnctype}>
          ^{formWidget}
          <input type=submit value="upload">
        In case of error: convert to UTF8 using vim like so:
        <br> :set fileencoding=utf8
        <br> :w <filename>
      |]

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

ppToTransaction :: (M.Map String String) -> Maybe Transaction
ppToTransaction mp = do
  date <- M.lookup "Date" mp 
  time <- M.lookup "Time" mp
  zone <- M.lookup "Time Zone" mp
  datetime <- F.parseTime defaultTimeLocale "%-m/%-d/%Y %H:%M:%S %Z" $
    date ++ " " ++ time ++ " " ++ zone
  name <- fmap T.pack $ M.lookup "Name" mp 
  fgamount <- fmap (\x -> read (filter ((/=) ',') x)) $ M.lookup "Gross" mp :: Maybe Float
  fnamount <- fmap (\x -> read (filter ((/=) ',') x)) $ M.lookup "Net" mp :: Maybe Float
  let gamount = truncate $ fgamount * 100.0
      namount = truncate $ fnamount * 100.0
  fromEmail <- fmap T.pack $ M.lookup "From Email Address" mp
  toEmail <- fmap T.pack $ M.lookup "To Email Address" mp
  transactionId <- fmap T.pack $ M.lookup "Transaction ID" mp
  Just $ Transaction datetime name gamount namount fromEmail toEmail transactionId

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

data Transaction = Transaction 
  { dateTime :: UTCTime
  , name :: Text
  , amountGross :: Int
  , amountNet :: Int
  , fromEmail :: Text
  , toEmail :: Text
  , transactionId :: Text
  }
  deriving Show

-- version where we create user accounts if they don't exist.
-- uh oh, user idents are sposed to be unique!!
addTransactionWUser :: UserId -> DuesRateId -> Transaction -> Handler (Maybe (Key Ledger))
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
    Ledger (Just (transactionId trans))
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


{-
-- version where we don't create user accounts.
addTransaction :: UserId -> Transaction -> Handler (Maybe (Key Ledger))
addTransaction creator trans = do 
  mbemail <- runDB $ getBy $ UniqueEmail (email trans)
  Entity ekey eml <- case mbemail of 
            Nothing -> do
              let eml = Email (email trans) Nothing Nothing
              key <- runDB $ insert eml
              return (Entity key eml) 
            Just eml -> return eml
  runDB $ insertUnique $ 
    Ledger (Just (transactionId trans))
           (emailUser eml)
           (Just ekey)
           (amountGross trans)
           (amountNet trans)
           creator
           False
           (dateTime trans)
           Nothing
-}

unMaybe :: Maybe a -> Handler a
unMaybe mba = 
  case mba of 
    Just mba -> return mba
    Nothing -> error "nothing"


postUtilitiesR :: Handler Html
postUtilitiesR = do
  logid <- requireAuthId
  requireAdmin logid
  mbdrid <- checkDuesRate "default" 0
  drid <- unMaybe mbdrid
  ((result, formWidget), formEnctype) <- runFormPost sampleForm
  let handlerName = "postUtilitiesR" :: Text
    in do
      case result of
        FormMissing -> error "form missing??"
        FormFailure meh -> error $ show meh
        FormSuccess fi -> do
          lift $ createDirectoryIfMissing True paypalDir
          now <- lift $ getCurrentTime
          let fname = paypalDir ++ "//paypal" ++ (show now)
          lift $ fileMove fi fname
          recs <- lift $ parsePaypal fname
          let transes = (MB.catMaybes (map ppToTransaction recs))
          keys <- mapM (addTransactionWUser logid drid) transes 
          let lrecs = length recs
              ltranses = length transes
              lkeys = length (MB.catMaybes keys)
          defaultLayout $ do
            aDomId <- newIdent
            setTitle "Yeeaaaahhh!"
            [whamlet|
              <br> #{show lrecs} records found in file.
              <br> #{show ltranses} records being valid-looking transactions.
              <br> we wrote #{show lkeys} transaction records.  
              <br> records with transaction IDs already in the database are skipped.
            |]
         


