module Util where

import Import
import Data.Fixed
import Data.Text.Read
import Data.Text

unMaybe :: Maybe a -> Handler a
unMaybe mba = 
  case mba of 
    Just mba -> return mba
    Nothing -> error "nothing"

unMaybeMsg :: Maybe a -> String -> Handler a
unMaybeMsg mba err = 
  case mba of 
    Just a -> return a
    Nothing -> error err

centiField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Centi
centiField = Field
    { fieldParse = parseHelper $ \s ->
        case Data.Text.Read.signed Data.Text.Read.rational s of
            Right (a, "") -> Right a
            _ -> Left $ MsgInvalidNumber s

    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="number" step=.01 :isReq:required="" value="#{showVal val}">
|]
    , fieldEnctype = UrlEncoded
    }
  where
    showVal = either id (pack . showC)
    showC x = show (x :: Centi)

