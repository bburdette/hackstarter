module Handler.User where

import Import
import Permissions
import UserForm
import Data.Time.Clock
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

data Meh = Meh { 
  wut :: String
  } 
  deriving Show

mehForm :: String -> Form Meh
mehForm wutt = renderDivs $ Meh 
  <$> pure wutt

getUserPermissions :: UserId -> Handler [(E.Value (KeyBackend E.SqlBackend Permission), E.Value Text)]
getUserPermissions uid = do 
  perms <- runDB $ E.select 
    $ E.from $ \(E.InnerJoin userpermission permission) -> do 
      E.where_ $ userpermission ^. UserPermissionUser E.==. (E.val uid)
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

getPermList :: UserId -> Bool -> Handler [(Text, Key Permission)]
getPermList uid isadmin = do
  case isadmin of 
    True -> do 
      perms <- getPermissions
      return $ permList perms
    False -> do 
      perms <- getUserPermissions uid
      return $ upermList perms

data PermId = PermId { 
  pid :: PermissionId
  }

addPermForm :: [(Text, Key Permission)] -> Form PermId
addPermForm permlist = renderDivs $ PermId
 <$> areq (selectFieldList permlist) "Add Permission" Nothing

getUserMaintR :: UserId -> UserId -> Handler Html
getUserMaintR logid userId = do
  admin <- isAdmin userId
  mbUser <- runDB $ get userId
  curtime <- lift getCurrentTime
  duesrates <- getDuesRates
  addpermissions <- getPermList logid admin
  userperms <- getUserPermissions userId
  case mbUser of 
    Nothing -> error "user id not found."
    Just userRec -> do 
      (formWidget, formEnctype) <- generateFormPost $ identifyForm "user" $ (userForm (utctDay curtime) (drList duesrates) (Just userRec))
      ((res, wutWidget), formEnctypee) <- runFormPost $ identifyForm "wut" $ (mehForm "nope")
      (permWidget, permEnctype) <- generateFormPost $ identifyForm "perm" $ addPermForm addpermissions 
      defaultLayout $ do 
        $(widgetFile "user")

-- readonly user view.
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
  case (admin || (logid == userId)) of
    True -> getUserMaintR logid userId
    False -> getUserRoR userId

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
                  identifyForm "user" (userForm (utctDay curtime) (drList duesrates) Nothing)   
          permissions <- getPermissions
          ((p_result, permWidget), permEnctype) 
              <- runFormPost $ identifyForm "perm" (addPermForm (permList permissions)) 
          ((w_result, wutWidget), formEnctype) 
              <- runFormPost $ identifyForm "wut" (mehForm "yep")
          case u_result of
            FormSuccess user -> do 
              del <- lookupPostParam "delete"
              sav <- lookupPostParam "save"
              case (del, sav) of 
                (Just _, _) -> do 
                  error "delete goes here"
                (_, Just _) -> do 
                  runDB $ replace uid user  
                  redirect UsersR
                _ -> do 
                  error "unknown button pressed probably"
            _ -> 
              case p_result of 
                FormSuccess perm -> do
                  res <- runDB $ insert $ UserPermission uid (pid perm) 
                  redirect $ UserR uid 
                _ -> 
                 case w_result of 
                  FormSuccess meh -> do
                    runDB $ delete uid
                    defaultLayout [whamlet| record deleted. #{show w_result}|]
                  _ -> error "error"
                     

