module Handler.AddUser where

import Import
import UserForm
import Data.Time.Clock
import Control.Monad

getAddUserR :: Handler Html
getAddUserR = do
  -- drs <- getDuesRates
  curtime <- lift getCurrentTime
  (userWidget, enctype) <- generateFormPost (userFormAdmin (utctDay curtime) Nothing)
  defaultLayout $ [whamlet|
    <h2> Add a new user:
    <form method=post enctype=#{enctype}>
      ^{userWidget}
      <input type=submit value=add user>|]

postAddUserR :: Handler Html
postAddUserR = do
  -- drs <- getDuesRates
  curtime <- lift getCurrentTime
  ((res, userWidget),enctype) <- runFormPost (userFormAdmin (utctDay curtime) Nothing)
  case res of
    FormSuccess user -> do
      usarID <- runDB $ insert user 
      defaultLayout [whamlet|
          added user record with id: #{(show usarID)}
          <br> <a href=@{UsersR}> Users 
          |]
    _ -> defaultLayout [whamlet|fale|]
