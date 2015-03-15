module Handler.ClubEmailRemove where

import Import
import Permissions

postClubEmailRemoveR :: ClubEmailId -> Handler Html
postClubEmailRemoveR ceid = do 
  logid <- requireAuthId 
  requireAdmin logid
  mbce <- runDB $ get ceid
  case mbce of 
    Just ce -> do 
      _ <- runDB $ delete ceid
      redirect $ ClubR (clubEmailClub ce)
    _ -> do
      error "record not found"
 
