module UserForm where

import Import
import Data.Time.Calendar

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

ornow :: Day -> Maybe User -> Maybe Day
ornow curday mbuser = 
  case mbuser of 
    Just user -> Just (userCreatedate user)
    Nothing -> Just curday

userFormSelf :: User -> Form User 
userFormSelf user = renderDivs $ User 
  <$> areq textField "handle" (Just $ userIdent user)
  <*> areq textField "name" (Just $ userName user)
  <*> aopt passwordField "Pwd" (Just $ userPassword user)
  <*> pure (userDuesrate user)
  <*> areq intField ("balance" { fsAttrs = [("readonly", "")] }) (Just $ userBalance user) 
  <*> areq dayField ("Create Date" { fsAttrs = [("readonly", "")] }) 
        (Just $ userCreatedate user) 

userFormAdmin :: Day -> [(Text, Key DuesRate)] -> Maybe User -> Form User 
userFormAdmin curday duesrates user = renderDivs $ User 
  <$> areq textField "handle" (userIdent <$> user)
  <*> areq textField "name" (userName <$> user)
  <*> aopt passwordField "Pwd" (userPassword <$> user)
  <*> areq (selectFieldList duesrates) "Dues rate" (userDuesrate <$> user)
  <*> areq intField ("balance" { fsAttrs = [("readonly", "")] }) (orzero user) 
  <*> areq dayField ("Create Date" { fsAttrs = [("readonly", "")] }) 
        (ornow curday user) 


