module Handler.AddUser where

import Import

getDuesRates :: HandlerT App IO [Entity DuesRate] 
getDuesRates = do
  blah <- runDB $ selectList [] []
  return blah

drList :: [(Entity DuesRate)] -> [(Text, Key DuesRate)]
drList = fmap (\(Entity blah vole) -> (duesRateName vole, blah))

-- newUserForm :: Form User
newUserForm duesrates = renderDivs $ User
 <$> areq textField "User ID" Nothing
 <*> aopt passwordField "Password" Nothing 
 <*> areq (selectFieldList duesrates) "Dues rate" Nothing

getAddUserR :: Handler Html
getAddUserR = do
  drs <- getDuesRates
  (userWidget, enctype) <- generateFormPost (newUserForm (drList drs))
  defaultLayout $ [whamlet|
    <h2> Add a new user:
    <form method=post enctype=#{enctype}>
      ^{userWidget}
      <input type=submit value=add user>|]

postAddUserR :: Handler Html
postAddUserR = do
  drs <- getDuesRates
  ((res, userWidget),enctype) <- runFormPost (newUserForm (drList drs))
  case res of
    FormSuccess user -> do
      usarID <- runDB $ insert user 
      defaultLayout [whamlet|
          added user record with id: #{(show usarID)}
          <br> <a href=@{UsersR}> Users 
          |]
    _ -> defaultLayout [whamlet|fale|]
