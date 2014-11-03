module Handler.LoginTest where

import Import

data LoginInfo = LoginInfo {
  ident :: Text 
}

loginForm :: Maybe Text -> Form LoginInfo
loginForm uident = renderDivs $ LoginInfo 
   <$> areq textField "User id:" uident

getLoginTestR :: Handler Html
getLoginTestR = do
  uident <- lookupSession "uidenelectFirst"
  (logwidg, enctype) <- generateFormPost $ loginForm uident
  defaultLayout $ [whamlet|
    <h5> Login
    <form method=post enctype=#{enctype}>
      ^{logwidg}
      <input type=submit value="ok">
    |]

{-
getaUser :: Maybe Text -> Handler (Maybe User)
getaUser ident = do 
  runDB $ select [UserIdent ==. ident]
-}

postLoginTestR :: Handler Html
postLoginTestR = do 
  ((res,widge),enctype) <- runFormPost $ loginForm Nothing
  case res of 
    FormSuccess loginfo -> do 
      mbusr <- runDB $ selectFirst [UserIdent ==. (ident loginfo)] []
      -- mbusr <- runDB $ get (ident loginfo)
      -- mbusr <- getaUser (ident loginfo) 
      case mbusr of 
        Just (Entity uid usr) -> do
          -- setSession "uid" uid 
          setSession "uident" (userIdent usr)
          redirect HomeR
        Nothing -> error "user not found"
        -- _ -> error "duplicate user idents??"
    _ -> error "form error"

