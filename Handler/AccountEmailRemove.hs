module Handler.AccountEmailRemove where

import Import
import Permissions

postAccountEmailRemoveR :: AccountEmailId -> Handler Html
postAccountEmailRemoveR aeid = do 
  logid <- requireAuthId 
  requireAdmin logid
  mbae <- runDB $ get aeid
  case mbae of 
    Just ae -> do 
      _ <- runDB $ delete aeid
      -- possible to redirect to caller??
      redirect $ ClubsR 
    _ -> do
      error "record not found"
 
