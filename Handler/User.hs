module Handler.User where

import Import
import UserForm
import Data.Time.Clock

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
  curtime <- lift getCurrentTime
  duesrates <- getDuesRates
  case mbUser of 
    Nothing -> error "user id not found."
    Just userRec -> do 
      (formWidget, formEnctype) <- generateFormPost $ identifyForm "user" $ (userForm (utctDay curtime) (drList duesrates) (Just userRec))
      ((res, wutWidget), formEnctypee) <- runFormPost $ identifyForm "wut" $ (mehForm "nope") 
      defaultLayout $ do 
        $(widgetFile "user")


postUserR :: UserId -> Handler Html
postUserR uid = 
    do
      duesrates <- getDuesRates
      curtime <- lift getCurrentTime
      ((u_result, formWidget), formEnctype) 
          <- runFormPost $
              identifyForm "user" (userForm (utctDay curtime) (drList duesrates) Nothing)   
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
               

