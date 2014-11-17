module Handler.User where

import Import
import Permissions
import UserForm
import Data.Time.Clock
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

getUserEmails :: UserId -> Handler [Entity Email]
getUserEmails uid = do 
  runDB $ selectList [EmailUser ==. Just uid] []

getUserPermissions :: UserId -> 
  Handler [(E.Value (KeyBackend E.SqlBackend Permission), 
            E.Value (KeyBackend E.SqlBackend UserPermission), 
            E.Value Text)]
getUserPermissions uid = do 
  perms <- runDB $ E.select 
    $ E.from $ \(E.InnerJoin userpermission permission) -> do 
      E.where_ $ userpermission ^. UserPermissionUser E.==. (E.val uid)
      E.where_ $ userpermission ^. UserPermissionPermission E.==. permission ^. PermissionId
      E.orderBy $ [E.asc ( permission ^. PermissionName )]
      return 
        ( permission ^. PermissionId,
          userpermission ^. UserPermissionId,
          permission ^. PermissionName ) 
  return perms

getUserAddPermissions :: UserId -> Handler [(E.Value (KeyBackend E.SqlBackend Permission), E.Value Text)]
getUserAddPermissions uid = do 
  perms <- runDB $ E.select 
    $ E.from $ \(E.InnerJoin userpermission permission) -> do 
      E.where_ $ userpermission ^. UserPermissionUser E.==. (E.val uid)
      E.where_ $ permission ^. PermissionUserAddable E.==. (E.val True)
      E.on $ userpermission ^. UserPermissionPermission E.==. permission ^. PermissionId
      E.orderBy $ [E.asc ( permission ^. PermissionName )]
      return 
        ( permission ^. PermissionId,
          permission ^. PermissionName ) 
  return perms

getPermissions :: HandlerT App IO [Entity Permission]
getPermissions = do 
  res <- runDB $ selectList [] []
  return res

permList :: [(Entity Permission)] -> [(Text, Key Permission)]
permList = fmap (\(Entity blah vole) -> (permissionName vole, blah))

upermList :: [(E.Value (KeyBackend E.SqlBackend Permission), E.Value Text)] -> [(Text, Key Permission)]
upermList = fmap (\(E.Value key, E.Value name) -> (name, key))

getPermAddList :: UserId -> Bool -> Handler [(Text, Key Permission)]
getPermAddList uid isadmin = do
  case isadmin of 
    True -> do 
      perms <- getPermissions
      return $ permList perms
    False -> do 
      perms <- getUserAddPermissions uid
      return $ upermList perms

data PermId = PermId { 
  pid :: PermissionId
  }

addPermForm :: [(Text, Key Permission)] -> Form PermId
addPermForm permlist = renderDivs $ PermId
 <$> areq (selectFieldList permlist) "Add Permission" Nothing

-- admin user maintenance.
-- admin can see, change dues rate.
-- admin can add permissions.
getUserAdminR :: UserId -> UserId -> Handler Html
getUserAdminR logid userId = do
  admin <- isAdmin logid
  mbUser <- runDB $ get userId
  curtime <- lift getCurrentTime
  duesrates <- getDuesRates
  addpermissions <- getPermAddList logid admin 
  userperms <- getUserPermissions userId
  useremails <- getUserEmails userId
  case mbUser of 
    Nothing -> error "user id not found."
    Just user -> do 
      (formWidget, formEnctype) <- generateFormPost $ identifyForm "user" $ (userFormAdmin (utctDay curtime) (drList duesrates) (Just user))
      (permWidget, permEnctype) <- generateFormPost $ identifyForm "perm" $ addPermForm addpermissions 
      defaultLayout $ do 
        $(widgetFile "user_admin")

-- users-viewing-users page.  
-- allows adding permissions to other users. 
getUserUserR :: UserId -> UserId -> Handler Html
getUserUserR logid userId = do
  admin <- isAdmin logid 
  mbUser <- runDB $ get userId
  curtime <- lift getCurrentTime
  duesrates <- getDuesRates
  addpermissions <- getPermAddList logid admin 
  userperms <- getUserPermissions userId
  case mbUser of 
    Nothing -> error "user id not found."
    Just user -> do 
      (permWidget, permEnctype) <- generateFormPost $ identifyForm "perm" $ addPermForm addpermissions 
      defaultLayout $ do 
        $(widgetFile "user_user")

-- self maint, non admin.
-- can change password, see permissions, see dues rate.
getUserSelfR :: UserId -> Handler Html
getUserSelfR userId = do
  admin <- isAdmin userId
  mbUser <- runDB $ get userId
  curtime <- lift getCurrentTime
  userperms <- getUserPermissions userId
  case mbUser of 
    Nothing -> error "user id not found."
    Just user -> do 
      (formWidget, formEnctype) <- generateFormPost $ identifyForm "user" $ (userFormSelf user)
      defaultLayout $ do 
        $(widgetFile "user_self")

-- read-only user view.  will we ever need this? 
getUserRoR :: UserId -> Handler Html
getUserRoR userId = do
  mbUser <- runDB $ get userId
  userperms <- getUserPermissions userId
  case mbUser of 
    Nothing -> error "user id not found."
    Just user -> do 
     defaultLayout $ do 
        $(widgetFile "user_ro")

getUserR :: UserId -> Handler Html
getUserR userId = do
  logid <- requireAuthId
  admin <- isAdmin logid
  case (admin, (logid == userId)) of
    (True, _) -> getUserAdminR logid userId
    (False, True) -> getUserSelfR userId
    (False, False) -> getUserUserR logid userId

postUserR :: UserId -> Handler Html
postUserR uid = 
    do
      logid <- requireAuthId
      admin <- isAdmin logid
      case (admin || (logid == uid)) of
        False -> error "not authorized" 
        True -> do 
          duesrates <- getDuesRates
          curtime <- lift getCurrentTime
          ((u_result, formWidget), formEnctype) 
              <- runFormPost $
                  identifyForm "user" (userFormAdmin (utctDay curtime) (drList duesrates) Nothing)   
          permissions <- getPermissions
          ((p_result, permWidget), permEnctype) 
              <- runFormPost $ identifyForm "perm" (addPermForm (permList permissions)) 
          case u_result of
            FormSuccess user -> do 
              del <- lookupPostParam "delete"
              sav <- lookupPostParam "save"
              case (del, sav) of 
                (Just _, _) -> do 
                  res <- runDB $ delete uid
                  defaultLayout [whamlet| record delete attempted: #{show res}|]
                (_, Just _) -> do 
                  runDB $ replace uid user  
                  redirect UsersR
                _ -> do 
                  error "unhandled form button"
            _ -> 
              case p_result of 
                FormSuccess perm -> do
                  res <- runDB $ insert $ UserPermission uid (pid perm) logid
                  redirect $ UserR uid 
                _ -> error "unhandled form"


