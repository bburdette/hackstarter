{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Users where

import Import
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

{-
getUsersR :: Handler Html
getUsersR = do
  theUsers <- runDB $ selectList [] [] 
  sess <- getSession
  defaultLayout $ do
    aDomId <- newIdent
    setTitle "Welcome To Userz!"
    $(widgetFile "users")
-}

getUsersR :: Handler Html
getUsersR = do
  users <- runDB $ E.select 
    $ E.from $ \(E.InnerJoin user duesRate) -> do 
      E.on $ user ^. UserDuesrate E.==. duesRate ^. DuesRateId
      return 
        ( user ^. UserId,
          user ^. UserIdent, 
          duesRate ^. DuesRateName, 
          duesRate ^. DuesRateAmount ) 
  defaultLayout $ do
    aDomId <- newIdent
    $(widgetFile "users")

{-
getUsersR :: Handler Html
getUsersR = do
   users <- runDB $ E.select 
      $ E.from $ \(E.InnerJoin user duesRate) -> do 
        E.on $ user ^. UserDuesrate E.==. duesRate ^. DuesRateId
        return 
          ( user ^. UserIdent, 
            user ^. UserDuesrate, 
            duesRate ^. DuesRateId, 
            duesRate ^. DuesRateName, 
            duesRate ^. DuesRateAmount ) 
   defaultLayout $ do
    [whamlet|
      <ul>
        <li> this is the first item
        $forall (E.Value a, E.Value b, E.Value c, E.Value d, E.Value e) <- users
          <li> #{a} #{d} #{e}
    |]
-}
 
--          <li> #{a} #{show b} #{show c} #{d} #{e}
