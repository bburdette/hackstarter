module Handler.User where

import Import

userForm :: User -> Form User 
userForm user = renderDivs $ User 
  <$> areq textField "name" (Just (userIdent user)) 
  <*> aopt passwordField "Pwd" (Just (userPassword user))

getUserR :: UserId -> Handler Html
getUserR userId = do
  mbUser <- runDB $ get userId
  case mbUser of 
    Nothing -> error "nothing"
    Just userRec -> do 
      (formWidget, formEnctype) <- generateFormPost (userForm userRec)
      defaultLayout $ do 
        $(widgetFile "user")
      -- defaultLayout $ [whamlet| ^{formWidget}|]

postUserR :: UserId -> Handler Html
postUserR uid = 
  let dummyuser = User "" Nothing in 
    do
      ((result, formWidget), formEnctype) <- runFormPost (userForm dummyuser)   
      case result of
        FormSuccess user -> do 
          runDB $ replace uid user  
          redirect UsersR
        _ -> error "user update fail"


