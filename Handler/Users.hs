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

userForm :: Html -> MForm Handler (FormResult User, Widget)
userForm = renderDivs $ User 
  <$> areq textField "Identtttt" Nothing
  <*> aopt passwordField "Pwd" Nothing

getUserR :: Handler Html
getUserR = do
    (formWidget, formEnctype) <- generateFormPost userForm
    let handlerName = "getUserR" :: Text
    defaultLayout
      [whamlet|
          <p> meh here's some crappe
          <form method=post action=@{UserR} enctype=#{formEnctype}>
            ^{formWidget}
            <p> some text
            <button>Submit
      |] 

postUserR :: Handler Html
postUserR = do
    ((result, formWidget), formEnctype) <- runFormPost userForm
    case result of 
      FormSuccess user -> do
        defaultLayout [whamlet|<p>#{show user}|]
      _ -> defaultLayout [whamlet|Invalid|]


