module Handler.DuesRates where

import Import
import Permissions

getDuesRatesR :: ClubId -> Handler Html
getDuesRatesR cid = do 
  theRates <- runDB $ selectList [DuesRateClub ==. cid] []
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

postDuesRatesR :: ClubId -> Handler Html
postDuesRatesR cid = error "Not yet implemented: postDuesRatesR"
