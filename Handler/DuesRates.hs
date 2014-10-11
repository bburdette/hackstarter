module Handler.DuesRates where

import Import

getDuesRatesR :: Handler Html
getDuesRatesR = do 
  theRates <- runDB $ selectList [] []
  defaultLayout $ do
    setTitle "ha ha"
    $(widgetFile "duesrates")

postDuesRatesR :: Handler Html
postDuesRatesR = error "Not yet implemented: postDuesRatesR"
