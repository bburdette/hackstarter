{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Users where

import Import
import Permissions
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

getUsersAdminR :: Handler Html
getUsersAdminR = do
  users <- runDB $ E.select 
    $ E.from $ \user -> do 
      return 
        ( user ^. UserId,
          user ^. UserIdent, 
          user ^. UserName 
        ) 
  defaultLayout $ do
    aDomId <- newIdent
    $(widgetFile "users")

getUsersNadminR :: Handler Html
getUsersNadminR = do
  users <- runDB $ selectList [] [] 
  defaultLayout $ do
    aDomId <- newIdent
    $(widgetFile "users_ro")

getUsersR :: Handler Html
getUsersR = do
  id <- requireAuthId
  admin <- isAdmin id
  case admin of 
    True -> getUsersAdminR
    False -> getUsersNadminR


