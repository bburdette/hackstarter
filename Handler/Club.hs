module Handler.Club where

import Import
import Permissions

data ClubForm = ClubForm
  {
  name :: Text
  }

clubForm :: Maybe Club -> Form Club
clubForm dr = renderDivs $ Club
 <$> areq textField "Club name" (clubName <$> dr) 

getClubR :: ClubId -> Handler Html
getClubR cid = do 
  login <- requireAuthId
  requireAdmin login
  mbclub <- runDB $ get cid
  case mbclub of 
    Just _ -> do
      (widge,enc) <- generateFormPost $ clubForm mbclub
      defaultLayout $ do
        [whamlet|
          Club Maintenance
          <form method=post enctype=#{enc}> 
            ^{widge}
            <input type=submit value=ok>
          <form method=post enctype=#{enc}>
            <input type=submit name="delete" value="delete">
        |]
    Nothing -> error $ "club id not found: " ++ show cid
      
postClubR :: ClubId -> Handler Html
postClubR cid = do 
  login <- requireAuthId
  requireAdmin login
  mbDel <- lookupPostParam "delete"
  case mbDel of 
    Just del -> do 
      _ <- runDB $ delete cid
      redirect ClubsR
    Nothing -> do 
      ((res, userWidget),enctype) <- runFormPost $ clubForm Nothing
      case res of 
        FormSuccess club -> do 
          runDB $ replace cid club
          redirect ClubsR
        _ -> error $ "some kind of problem "

{-
postDuesRateR :: DuesRateId -> Handler Html
postDuesRateR dri = do
  _ <- requireAuthId
  mbDel <- lookupPostParam "delete"
  case mbDel of 
    Just del -> do 
      _ <- runDB $ do 
        delete dri 
      redirect DuesRatesR
    Nothing -> do 
      ((res,widg),enctype) <- runFormPost (duesRateForm Nothing)
      case res of 
        FormSuccess dr -> do 
          _ <- runDB $ replace dri dr
          redirect DuesRatesR
        _ -> error "bad format error"
-} 
