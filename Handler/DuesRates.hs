module Handler.DuesRates where

import Import

getDuesRatesR :: Handler Html
getDuesRatesR = do 
  theRates <- runDB $ selectList [] []
  defaultLayout $ do
    setTitle "Dues Rates"
    $(widgetFile "duesrates")

postDuesRatesR :: Handler Html
postDuesRatesR = error "Not yet implemented: postDuesRatesR"
