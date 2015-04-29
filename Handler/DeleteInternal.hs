module Handler.DeleteInternal where

import Import
import Permissions

getDeleteInternalR :: InternalId -> AccountId -> Handler Html
getDeleteInternalR iid aid = do 
  logid <- requireAuthId
  requireAdmin logid
  runDB $ delete iid
  redirect $ AccountR aid
