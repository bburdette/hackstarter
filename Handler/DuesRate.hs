module Handler.DuesRate where

import Import
import DuesRateForm

getDuesRateR :: DuesRateId -> Handler Html
getDuesRateR dri = do 
  mbDr <- runDB $ get dri
  case mbDr of 
    Nothing -> error "duesrate not found"
    Just dr -> do 
      (widg, enctype) <- generateFormPost (duesRateForm mbDr)
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
  mbDel <- lookupPostParam "delete"
  case mbDel of 
    Just del -> do 
      _ <- runDB $ delete dri 
      redirect DuesRatesR
    Nothing -> do 
      ((res,widg),enctype) <- runFormPost (duesRateForm Nothing)
      case res of 
        FormSuccess dr -> do 
          _ <- runDB $ replace dri dr
          redirect DuesRatesR
        _ -> error "bad format error"
   
