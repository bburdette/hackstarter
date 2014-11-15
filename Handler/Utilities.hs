module Handler.Utilities where

import Import
import System.IO
import Data.Conduit
import Data.ByteString
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB

getUtilitiesR :: Handler Html
getUtilitiesR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getUtilitiesR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        [whamlet|
          <form method=post enctype=#{formEnctype}>
            ^{formWidget}
            <input type=submit value="upload">
        |]

mahsink :: Sink ByteString IO ()
mahsink = CL.mapM_ Data.ByteString.putStrLn


postUtilitiesR :: Handler Html
postUtilitiesR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postUtilitiesR" :: Text
      in do
        case result of
          FormMissing -> error "form missing??"
          FormFailure meh -> error $ show meh
          FormSuccess (fi,txt) -> do
            -- blah <- openFile txt WriteMode
            -- liftIO $ writeFile (unpack txt) (fileSource fi)
            -- lift $ (fileSource fi) =$ mahsink
            -- lift $ (fileSource fi) =$ (CB.sinkFile "tst.txt")
            lift $ fileMove fi "yeah.txt" 
            defaultLayout $ do
              aDomId <- newIdent
              setTitle "Welcome To Yesod!"
              [whamlet|
                we wrote eet!
              |]
           

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing


