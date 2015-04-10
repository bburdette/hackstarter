module Handler.DuesRate where

import Import
import DuesRateForm
import Permissions
import Util

getDuesRateR :: DuesRateId -> Handler Html
getDuesRateR dri = do 
  -- todo: require admin
  id <- requireAuthId
  admin <- isAdmin id
  mbDr <- runDB $ get dri
  dr <- unMaybe mbDr
  case admin of 
    False -> redirect $ DuesRatesR (duesRateClub dr)
    True -> do 
      (widg, enctype) <- generateFormPost (duesRateForm (duesRateClub dr) mbDr)
      defaultLayout $ do
        [whamlet| 
          <form method=post enctype=#{enctype}>
            ^{widg}
            <input type=submit value="save changes">
          <form method=post enctype=#{enctype}>
            <input type=submit name="delete" value="delete">
        |]

postDuesRateR :: DuesRateId -> Handler Html
postDuesRateR dri = do
  login <- requireAuthId
  requireAdmin login
  mbDr <- runDB $ get dri
  dr <- unMaybe mbDr
  mbDel <- lookupPostParam "delete"
  case mbDel of 
    Just del -> do 
      _ <- runDB $ delete dri 
      redirect $ DuesRatesR (duesRateClub dr)
    Nothing -> do
      ((res,widg),enctype) <- runFormPost 
                                (duesRateForm (duesRateClub dr) (Just dr))
      case res of 
        FormSuccess dr -> do 
          _ <- runDB $ replace dri dr
          redirect $ DuesRatesR (duesRateClub dr)
        _ -> error "bad format error"
   
