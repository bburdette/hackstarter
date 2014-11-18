module Permissions where

import Import
import Data.Time.Clock
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

hasPermission :: UserId -> PermissionId -> Handler Bool 
hasPermission uid pid = do
  meh <- runDB $ selectFirst [UserPermissionUser ==. uid
                            , UserPermissionPermission ==. pid] []
  case meh of 
    Nothing -> return False
    _ -> return True

{-
checkPermission :: Text -> Handler (Maybe PermissionId)
checkPermission pname = do 
  meh <- getBy $ PermissionUniqueName pname
  case meh of 
    Just (Entity pid perm) -> return (Just pid)
    _ -> return Nothing
-}

getPermissionId :: Text -> Handler (Maybe PermissionId)
getPermissionId pname = do 
  meh <- runDB $ getBy $ PermissionUniqueName pname
  case meh of 
    Just (Entity pid perm) -> return (Just pid)
    _ -> return Nothing

isAdmin :: UserId -> Handler Bool
isAdmin uid = do 
  pid <- getPermissionId "admin"
  maybe (return False) (hasPermission uid) pid

requireAdmin :: UserId -> Handler ()
requireAdmin uid = do
  admin <- isAdmin uid
  case admin of 
    True -> return ()
    False -> redirect HomeR


{- 
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
-}

getPermissionUserIds :: PermissionId -> Handler [UserId]
getPermissionUserIds pid = do 
  users <- runDB $ E.select 
    $ E.from $ \(E.InnerJoin userpermission user) -> do 
      E.where_ $ userpermission ^. UserPermissionPermission E.==. (E.val pid)
      E.on $ userpermission ^. UserPermissionUser E.==. user ^. UserId 
      E.orderBy $ [E.asc ( user ^. UserIdent )]
      return 
        ( user ^. UserId ) 
  return $ fmap (\(E.Value uid) -> uid) users 

--isAdmin :: UserId -> Handler Bool
--isAdmin uid = hasPermission uid 1
--  do 
--      permId <- runDB $ insert $ Permission permname Nothing
--      return permId

checkPermission :: Text -> Bool -> Handler (Maybe PermissionId)
checkPermission permname userAddable = do 
  meh <- runDB $ getBy $ PermissionUniqueName permname
  case meh of 
    Nothing -> 
      do 
        permId <- runDB $ insert $ Permission permname Nothing userAddable
        return $ Just permId
    Just (Entity key wha) -> return $ Just key
  
-- check if there's a dues rate with the indicated name.
-- if not, create one with that name and the passed in amount.
checkDuesRate :: Text -> Int -> Handler (Maybe DuesRateId)
checkDuesRate drname amount = do 
  drent <- runDB $ getBy $ UniqueDuesRate drname
  case drent of 
    Nothing -> 
      do 
        drId <- runDB $ insert $ DuesRate drname amount
        return $ Just drId
    Just (Entity drId dr) -> 
      return $ Just drId

-- make sure there's an admin permission, and an admin user.
checkAdmin :: Handler (Maybe UserId)
checkAdmin = do 
  mbpid <- checkPermission "admin" False
  case mbpid of 
    Just pid -> do
      admin <- runDB $ selectFirst [UserPermissionPermission ==. pid] [] 
      case admin of 
        Nothing -> do 
          -- add an admin user.
          mbdrid <- checkDuesRate "default" 0
          case mbdrid of 
            Just drid -> do 
              curtime <- lift getCurrentTime
              uid <- runDB $ insert $ User "admin" Nothing drid 0 (utctDay curtime)
              upid <- runDB $ insert $ UserPermission uid pid uid
              return $ Just uid
            _ -> return Nothing
        Just (Entity upid up) -> return $ Just (userPermissionUser up)
    _ -> return Nothing


