module DuesRateForm where

import Import

import Util

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


duesRateForm :: ClubId -> Maybe DuesRate -> Form DuesRate
duesRateForm cid dr = renderDivs $ DuesRate
 <$> areq textField "Dues rate name" (duesRateName <$> dr) 
 <*> areq centiField "Dues rate amount" (duesRateAmount <$> dr)
 <*> areq hiddenField "" (Just cid)

