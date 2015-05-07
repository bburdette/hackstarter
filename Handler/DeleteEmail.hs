module Handler.DeleteEmail where

import Import
import Permissions
import Util

dellerSessKey :: Text
dellerSessKey = "edeller"

getDeleteEmailR :: EmailId -> Handler Html
getDeleteEmailR eid = do 
  login <- requireAuthId
  requireAdmin login
  runDB $ delete eid
  redirectSessUrl dellerSessKey HomeR

