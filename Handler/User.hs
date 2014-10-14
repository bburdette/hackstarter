module Handler.User where

import Import

getDuesRates :: HandlerT App IO [Entity DuesRate] 
getDuesRates = do
  blah <- runDB $ selectList [] []
  return blah

drList :: [(Entity DuesRate)] -> [(Text, Key DuesRate)]
drList = fmap (\(Entity blah vole) -> (duesRateName vole, blah))

userForm :: [(Text, Key DuesRate)] -> Maybe User -> Form User 
userForm duesrates user = renderDivs $ User 
  <$> areq textField "name" (userIdent <$> user)
  <*> aopt passwordField "Pwd" (userPassword <$> user)
  <*> areq (selectFieldList duesrates) "Dues rate" (userDuesrate <$> user)


getUserR :: UserId -> Handler Html
getUserR userId = do
  mbUser <- runDB $ get userId
  duesrates <- getDuesRates
  case mbUser of 
    Nothing -> error "nothing"
    Just userRec -> do 
      (formWidget, formEnctype) <- generateFormPost (userForm (drList duesrates) (Just userRec))
      defaultLayout $ do 
        $(widgetFile "user")

postUserR :: UserId -> Handler Html
postUserR uid = 
    do
      duesrates <- getDuesRates
      ((result, formWidget), formEnctype) 
          <- runFormPost 
              (userForm (drList duesrates) Nothing)   
      case result of
        FormSuccess user -> do 
          runDB $ replace uid user  
          redirect UsersR
        _ -> error "user update fail"

