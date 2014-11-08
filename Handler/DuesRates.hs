module Handler.DuesRates where

import Import
import Permissions

getDuesRatesR :: Handler Html
getDuesRatesR = do 
  theRates <- runDB $ selectList [] []
  id <- requireAuthId
  admin <- isAdmin id
  case admin of 
    True -> do 
      defaultLayout $ do
        setTitle "Dues Rates"
        $(widgetFile "duesrates")
    False -> do 
      defaultLayout $ do
        setTitle "Dues Rates"
        $(widgetFile "duesrates_ro")

postDuesRatesR :: Handler Html
postDuesRatesR = error "Not yet implemented: postDuesRatesR"
