module Handler.ClubAccountDelete where

import Import
import Permissions

postClubAccountDeleteR :: ClubAccountId -> Handler Html
postClubAccountDeleteR caid = do 
  logid <- requireAuthId 
  requireAdmin logid
  mbca <- runDB $ get caid
  case mbca of 
    Just ca -> do 
      _ <- runDB $ delete caid
      redirect $ ClubR (clubAccountClub ca)
    _ -> do
      error "record not found"
 
