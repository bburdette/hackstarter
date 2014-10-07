{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Users where

import Import

getUsersR :: Handler Html
getUsersR = do
  theUsers <- runDB $ selectList [] [] 
  sess <- getSession
  defaultLayout $ do
      aDomId <- newIdent
      setTitle "Welcome To Userz!"
      $(widgetFile "users")


