module Handler.Permission where

import Import
import PermissionForm
import Permissions

import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

getPermissionAdminR :: PermissionId -> Handler Html
getPermissionAdminR pid = do
  mbperm <- runDB $ get pid
  case mbperm of 
    Nothing -> error "record not found"
    Just perm -> do
      users <- getPermissionUsers pid
      (pwidget, enctype) <- generateFormPost $ permissionForm mbperm
      defaultLayout $ [whamlet|
      <h3> Edit permission
      <form method=post enctype=#{enctype}>
        ^{pwidget}
        <input type=submit value=Ok>
      <table>
        <tr>
          <th> Users with this permission:
        $forall (E.Value uid, E.Value uident) <- users
          <tr>
            <td> 
              <a href=@{UserR uid}> #{uident}
      |]

getPermissionR :: PermissionId -> Handler Html
getPermissionR pid = do
  loguid <- requireAuthId
  admin <- isAdmin loguid
  case admin of 
    True -> getPermissionAdminR pid
    False -> getPermissionRoR pid

getPermissionRoR :: PermissionId -> Handler Html
getPermissionRoR pid = do
  mbperm <- runDB $ get pid
  case mbperm of 
    Nothing -> error "record not found"
    Just perm -> do
      users <- getPermissionUsers pid
      defaultLayout $ [whamlet|
      <h3> Permission:  #{permissionName perm}
      <table>
        <tr>
          <th> Users with this permission:
        $forall (E.Value uid, E.Value uident) <- users
          <tr>
            <td> 
              <a href=@{UserR uid}> #{uident}
      |]


postPermissionR :: PermissionId -> Handler Html
postPermissionR pid = do
  loguid <- requireAuthId
  admin <- isAdmin loguid
  requireAdmin admin
  ((res,_),enctype) <- runFormPost $ permissionForm Nothing
  case res of 
    FormSuccess perm -> do 
      runDB $ replace pid perm
      redirect PermissionsR
    _ -> error "record edit failed"
        
