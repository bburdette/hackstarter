module UserForm where

import Import

getDuesRates :: HandlerT App IO [Entity DuesRate] 
getDuesRates = do
  blah <- runDB $ selectList [] []
  return blah

drList :: [(Entity DuesRate)] -> [(Text, Key DuesRate)]
drList = fmap (\(Entity blah vole) -> (duesRateName vole, blah))

orzero :: Maybe User -> Maybe Int
orzero mbuser = 
  case mbuser of
    Nothing -> Just 0
    _ -> userBalance <$> mbuser

userForm :: [(Text, Key DuesRate)] -> Maybe User -> Form User 
userForm duesrates user = renderDivs $ User 
  <$> areq textField "name" (userIdent <$> user)
  <*> aopt passwordField "Pwd" (userPassword <$> user)
  <*> areq (selectFieldList duesrates) "Dues rate" (userDuesrate <$> user)
  <*> areq intField ("balance" { fsAttrs = [("readonly", "")] }) (orzero user) 


