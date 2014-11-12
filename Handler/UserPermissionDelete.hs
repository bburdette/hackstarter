module Handler.UserPermissionDelete where

import Import
import Permissions

postUserPermissionDeleteR :: UserPermissionId -> Handler Html
postUserPermissionDeleteR upid = do 
  logid <- requireAuthId 
  admin <- isAdmin logid
  mbup <- runDB $ get upid
  case mbup of 
    Just up -> case admin || (userPermissionUser up) == logid of 
       True -> do 
          res <- runDB $ delete upid
          defaultLayout $ [whamlet| attempted delete.  result: #{show res}|]
       False -> do
          error "unauthorized"
    _ -> do
      error "record not found"
  
