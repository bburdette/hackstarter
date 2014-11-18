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
    fileAFormReq "Upload paypal transaction file:"

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
      setTitle "Welcome To Yesod!"
      [whamlet|
        <form method=post enctype=#{formEnctype}>
          ^{formWidget}
          <input type=submit value="upload">
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
  famount <- fmap (\x -> read (filter ((/=) ',') x)) $ M.lookup "Net" mp :: Maybe Float
  let amount = truncate $ famount * 100.0
  email <- fmap T.pack $ M.lookup "From Email Address" mp
  transactionId <- fmap T.pack $ M.lookup "Transaction ID" mp
  Just $ Transaction datetime name amount email transactionId

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
  , amount :: Int
  , email :: Text
  , transactionId :: Text
  }
  deriving Show

-- version where we create user accounts if they don't exist.

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
           (amount trans)
           creator
           (dateTime trans)

postUtilitiesR :: Handler Html
postUtilitiesR = do
  logid <- requireAuthId
  requireAdmin logid 
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
          keys <- mapM (addTransaction logid) transes 
          let lrecs = length recs
              ltranses = length transes
              lkeys = length (MB.catMaybes keys)
          defaultLayout $ do
            aDomId <- newIdent
            setTitle "Yeeaaaahhh!"
            [whamlet|
              <br> #{show lrecs} records found in file.
              <br> #{show ltranses} records being valid-looking transactions.found in file.
              <br> we wrote #{show lkeys} transaction records.  
              <br> records with duplicate transaction IDs are skipped.
            |]
         


