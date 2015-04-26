{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Users where

import Import hiding ((==.), (!=.), (||.))
import Database.Esqueleto
import qualified Database.Persist as P
import Permissions

getUsersAdminR :: Handler Html
getUsersAdminR = do
  users <- runDB $ select 
    $ from $ \user -> do 
      orderBy [asc $ user ^. UserName] 
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
  users <- runDB $ P.selectList [] [(P.Asc UserName)] 
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


