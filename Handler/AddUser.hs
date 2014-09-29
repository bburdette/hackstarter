module Handler.AddUser where

import Import

newUserForm :: Form User
newUserForm = renderDivs $ User
 <$> areq textField "User ID" Nothing
 <*> aopt passwordField "Password" Nothing 

getAddUserR :: Handler Html
getAddUserR = do
  (userWidget, enctype) <- generateFormPost newUserForm
  defaultLayout $ [whamlet|
    <h2> Add a new user:
    <form method=post enctype=#(enctype}>
      ^{userWidget}
      <input type=submit value=add user>|]

postAddUserR :: Handler Html
postAddUserR = do
  ((res, userWidget),enctype) <- runFormPost newUserForm
  case res of
    FormSuccess user -> do
      usarID <- runDB $ insert user 
      defaultLayout [whamlet|
          added user record with id: #{(show usarID)}
          <br> <a href=@{UsersR}> Users 
          |]
    _ -> defaultLayout [whamlet|fale|] 
