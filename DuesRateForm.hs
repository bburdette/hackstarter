module DuesRateForm where

import Import

import Data.Fixed
import Data.Text.Read
import Data.Text

{-

intField :: (Monad m, Integral i, RenderMessage (HandlerSite m) FormMessage) => Field m i
intField = Field
    { fieldParse = parseHelper $ \s ->
        case Data.Text.Read.signed Data.Text.Read.decimal s of
            Right (a, "") -> Right a
            _ -> Left $ MsgInvalidInteger s

    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="number" step=1 :isReq:required="" value="#{showVal val}">
|]
    , fieldEnctype = UrlEncoded
    }
  where
    showVal = either id (pack . showI)
    showI x = show (fromIntegral x :: Integer)

-}


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




duesRateForm :: Maybe DuesRate -> Form DuesRate
duesRateForm dr = renderDivs $ DuesRate
 <$> areq textField "Dues rate name" (duesRateName <$> dr) 
 <*> areq centiField "Dues rate amount" (duesRateAmount <$> dr)

