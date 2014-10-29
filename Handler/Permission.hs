module Handler.Permission where

import Import
import PermissionForm

getPermissionR :: PermissionId -> Handler Html
getPermissionR pid = do
  mbperm <- runDB $ get pid
  case mbperm of 
    Nothing -> error "record not found"
    Just perm -> do
      (pwidget, enctype) <- generateFormPost $ permissionForm mbperm
      defaultLayout $ [whamlet|
      <form method=post enctype=#{enctype}>
        ^{pwidget}
        <input type=submit value=Ok>
      |]

postPermissionR :: PermissionId -> Handler Html
postPermissionR = error "Not yet implemented: postPermissionR"
