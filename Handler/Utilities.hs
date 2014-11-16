module Handler.Utilities where

import Import
import System.IO
import Data.Conduit
import qualified Data.Text as T
import System.Directory

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

postUtilitiesR :: Handler Html
postUtilitiesR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postUtilitiesR" :: Text
      in do
        case result of
          FormMissing -> error "form missing??"
          FormFailure meh -> error $ show meh
          FormSuccess (fi,txt) -> do
            lift $ createDirectory paypalDir
            lift $ fileMove fi $ paypalDir ++ "//" ++ (T.unpack txt)
            defaultLayout $ do
              aDomId <- newIdent
              setTitle "Welcome To Yesod!"
              [whamlet|
                we wrote eet!
              |]
           


