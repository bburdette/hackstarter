module Handler.Permissions where

import Import
import PermissionForm
import Permissions

getPermissionsR :: Handler Html
getPermissionsR = do
  loguid <- requireAuthId
  admin <- isAdmin loguid
  case admin of 
    True -> getPermissionsAdminR
    False -> getPermissionsRoR

getPermissionsAdminR :: Handler Html
getPermissionsAdminR = do
  perms <- runDB $ selectList [] []
  (perform, enctype) <- generateFormPost $ permissionForm Nothing
  defaultLayout $ [whamlet|
    <table>
      <tr>
        <th> Permissions
      $forall (Entity pid perm) <- perms
        <tr>
          <td> 
             <a href=@{PermissionR pid}> #{ permissionName perm }
    <h4>add permission
    <form method=post enctype=#{enctype}>
      ^{perform}
      <input type=submit value=add>
    |]

getPermissionsRoR :: Handler Html
getPermissionsRoR = do
  perms <- runDB $ selectList [] []
  defaultLayout $ [whamlet|
    <table>
      <tr>
        <th> Permissions
      $forall (Entity pid perm) <- perms
        <tr>
          <td> 
             <a href=@{PermissionR pid}> #{ permissionName perm }
    |]

postPermissionsR :: Handler Html
postPermissionsR = do
  loguid <- requireAuthId
  admin <- isAdmin loguid
  case admin of 
    False -> error "not authorized"
    True -> do 
      ((res, permWidget),enctype) <- runFormPost $ permissionForm Nothing
      case res of 
        FormSuccess perm -> do
          permID <- runDB $ insert perm
          redirect PermissionsR
        _ -> defaultLayout [whamlet|fale!|]

