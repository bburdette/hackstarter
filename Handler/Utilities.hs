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
import Data.List
import Data.Char
import System.Locale

paypalDir = "paypals"

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Upload paypal transaction file:"
    <*> areq textField "Save as: " Nothing

getUtilitiesR :: Handler Html
getUtilitiesR = do
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
          let heads = fmap (dropWhile isSpace) $ head csvl
              zipmap heads datas = M.fromList $ zip heads datas
          return $ fmap (zipmap heads) (tail csvl)
    Left err -> return []

data Transaction = Transaction 
  { dateTime :: UTCTime
  , name :: Text
  , amount :: Int
  , email :: Text
  , transactionId :: Text
  }
  deriving Show

ppToTransaction :: (M.Map String String) -> Maybe Transaction
ppToTransaction mp = do
  date <- M.lookup "Date" mp 
  time <- M.lookup "Time" mp
  zone <- M.lookup "Time Zone" mp
  datetime <- F.parseTime defaultTimeLocale "%m/%d/%Y %H:%M:%S %Z" $
    date ++ " " ++ time ++ " " ++ zone
  name <- fmap T.pack $ M.lookup "Name" mp 
  famount <- fmap read $ M.lookup "Net" mp :: Maybe Float
  let amount = truncate $ famount * 100.0
  email <- fmap T.pack $ M.lookup "From Email Address" mp
  transactionId <- fmap T.pack $ M.lookup "Transaction ID" mp
  Just $ Transaction datetime name amount email transactionId

postUtilitiesR :: Handler Html
postUtilitiesR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postUtilitiesR" :: Text
      in do
        case result of
          FormMissing -> error "form missing??"
          FormFailure meh -> error $ show meh
          FormSuccess (fi,txt) -> do
            lift $ createDirectoryIfMissing True paypalDir
            now <- lift $ getCurrentTime
            let fname = paypalDir ++ "//paypal" ++ (show now)-- (T.unpack txt)
            lift $ fileMove fi fname
            recs <- lift $ parsePaypal fname
            defaultLayout $ do
              aDomId <- newIdent
              setTitle "Welcome To Yesod!"
              [whamlet|
                we wrote eet!
              |]
           


