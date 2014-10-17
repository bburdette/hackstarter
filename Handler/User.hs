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

data Meh = Meh { 
  wut :: String
  } 
  deriving Show

mehForm :: String -> Form Meh
mehForm wutt = renderDivs $ Meh 
  <$> pure wutt

getUserR :: UserId -> Handler Html
getUserR userId = do
  mbUser <- runDB $ get userId
  duesrates <- getDuesRates
  case mbUser of 
    Nothing -> error "user id not found."
    Just userRec -> do 
      (formWidget, formEnctype) <- generateFormPost $ identifyForm "user" $ (userForm (drList duesrates) (Just userRec))
      ((res, wutWidget), formEnctypee) <- runFormPost $ identifyForm "wut" $ (mehForm "nope") 
      defaultLayout $ do 
        $(widgetFile "user")



postUserR :: UserId -> Handler Html
postUserR uid = 
    do
      duesrates <- getDuesRates
      ((u_result, formWidget), formEnctype) 
          <- runFormPost $
              identifyForm "user" (userForm (drList duesrates) Nothing)   
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
          case w_result of 
            FormSuccess meh -> do
              runDB $ delete uid
              defaultLayout [whamlet| record deleted. #{show w_result}|]
            _ -> error "error"
               

