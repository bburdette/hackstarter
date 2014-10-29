module Handler.Permission where

import Import
import PermissionForm
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))


getPermissionUsers :: PermissionId -> 
  Handler [(E.Value (KeyBackend E.SqlBackend User), E.Value Text)]
getPermissionUsers pid = do 
  users <- runDB $ E.select 
    $ E.from $ \(E.InnerJoin userpermission user) -> do 
      E.where_ $ userpermission ^. UserPermissionPermission E.==. (E.val pid)
      E.on $ userpermission ^. UserPermissionUser E.==. user ^. UserId 
      E.orderBy $ [E.asc ( user ^. UserIdent )]
      return 
        ( user ^. UserId,
          user ^. UserIdent ) 
  return users


getPermissionR :: PermissionId -> Handler Html
getPermissionR pid = do
  mbperm <- runDB $ get pid
  case mbperm of 
    Nothing -> error "record not found"
    Just perm -> do
      users <- getPermissionUsers pid
      (pwidget, enctype) <- generateFormPost $ permissionForm mbperm
      defaultLayout $ [whamlet|
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

postPermissionR :: PermissionId -> Handler Html
postPermissionR pid = do
  ((res,_),enctype) <- runFormPost $ permissionForm Nothing
  case res of 
    FormSuccess perm -> do 
      runDB $ replace pid perm
      redirect PermissionsR
    _ -> error "record edit failed"
  
