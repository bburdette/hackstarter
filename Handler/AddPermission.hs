module Handler.AddPermission where

import Import
import PermissionForm
import Permissions

getAddPermissionR :: Handler Html
getAddPermissionR = do
  (pwidget, enctype) <- generateFormPost $ permissionForm Nothing 
  defaultLayout $ [whamlet|
    <h3> Add new permission
    <form method=post enctype=#{enctype}>
      ^{pwidget}
      <input type=submit value=Add>
   |]

postAddPermissionR :: Handler Html
postAddPermissionR = do 
  loguid <- requireAuthId
  admin <- isAdmin loguid
  case admin of 
    False -> error "not authorized"
    True -> do  
      ((res,_),enctype) <- runFormPost $ permissionForm Nothing
      case res of 
        FormSuccess perm -> do 
          runDB $ insert perm
          redirect PermissionsR
        _ -> error "record add failed"
 
