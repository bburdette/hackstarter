{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Users where

import Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
{-
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing

-}

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing

getUsersR :: Handler Html
getUsersR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getUsersR" :: Text
        googlink = "https://www.google.com" :: Text
        itemz = [0..10] :: [Int]
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Userz!"
        $(widgetFile "users")


data Usah = Usah
 { uName :: Text
 , uRank :: Text
 , uSerialNumber :: Int
 }
 deriving Show 

usahForm :: Html -> MForm Handler (FormResult Usah, Widget)
usahForm = renderDivs $ Usah 
  <$> areq textField "Name" Nothing
  <*> areq textField "Rank" Nothing
  <*> areq intField "Serial Number" Nothing

getUsahR :: Handler Html
getUsahR = do
    (formWidget, formEnctype) <- generateFormPost usahForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getUsahR" :: Text
    defaultLayout
      [whamlet|
          <p> meh here's some crappe
          <form method=post action=@{UsahR} enctype=#{formEnctype}>
            ^{formWidget}
            <p> some text
            <button>Submit
      |] 

postUsahR :: Handler Html
postUsahR = do
    ((result, formWidget), formEnctype) <- runFormPost usahForm
    case result of 
      FormSuccess usah -> defaultLayout [whamlet|<p>#{show usah}|]
      _ -> defaultLayout [whamlet|Invalid|]

{-
User
    ident Text
    password Text Maybe
    UniqueUser ident
    deriving Typeable
-}

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
      FormSuccess user -> defaultLayout [whamlet|<p>#{show user}|]
      _ -> defaultLayout [whamlet|Invalid|]


