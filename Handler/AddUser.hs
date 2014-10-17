module Handler.AddUser where

import Import
import UserForm

getAddUserR :: Handler Html
getAddUserR = do
  drs <- getDuesRates
  (userWidget, enctype) <- generateFormPost (userForm (drList drs) Nothing)
  defaultLayout $ [whamlet|
    <h2> Add a new user:
    <form method=post enctype=#{enctype}>
      ^{userWidget}
      <input type=submit value=add user>|]

postAddUserR :: Handler Html
postAddUserR = do
  drs <- getDuesRates
  ((res, userWidget),enctype) <- runFormPost (userForm (drList drs) Nothing)
  case res of
    FormSuccess user -> do
      usarID <- runDB $ insert user 
      defaultLayout [whamlet|
          added user record with id: #{(show usarID)}
          <br> <a href=@{UsersR}> Users 
          |]
    _ -> defaultLayout [whamlet|fale|]
