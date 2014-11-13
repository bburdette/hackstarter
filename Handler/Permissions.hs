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
  defaultLayout $ [whamlet|
    <table>
      <tr>
        <th> Permissions
      $forall (Entity pid perm) <- perms
        <tr>
          <td> 
             <a href=@{PermissionR pid}> #{ permissionName perm }
    <a href=@{AddPermissionR} > add new permission
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
postPermissionsR = error "unimplemented"
{-
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
-}
