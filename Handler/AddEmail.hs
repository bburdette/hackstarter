module Handler.AddEmail where

import Import
import Permissions

data Eml = Eml
  {
  email :: Text
  }

addEmailForm :: Form Eml
addEmailForm = renderDivs $ Eml
 <$> areq emailField "Add Email:" Nothing

getAddEmailR :: UserId -> Handler Html
getAddEmailR uid = do 
  login <- requireAuthId
  admin <- isAdmin login
  requireBool (admin || (uid == login))
  (formWidge, enctype) <- generateFormPost addEmailForm
  defaultLayout $ do 
    [whamlet|
      Add email for user #{ show uid }
      <form method=post enctype=#{enctype}>
        ^{formWidge}
        <input type=submit value=add email>
      |] 

postAddEmailR :: UserId -> Handler Html
postAddEmailR uid = do
  login <- requireAuthId
  admin <- isAdmin login
  requireBool (admin || (uid == login))
  ((res, userWidget),enctype) <- runFormPost addEmailForm 
  case res of
    FormSuccess eml -> do
      mbeml <- runDB $ getBy $ UniqueEmail (email eml)
      case mbeml of 
        Just (Entity eid eml) -> 
          case (emailUser eml) of 
            Nothing -> do 
              _ <- runDB $ replace eid $ eml { emailUser = Just uid }
              redirect $ UserR uid
            _ -> error "unauthorized"
        Nothing -> do
          _ <- runDB $ insert $ Email (email eml) (Just uid) Nothing
          redirect $ UserR uid
    _ -> defaultLayout [whamlet|fale|]  
