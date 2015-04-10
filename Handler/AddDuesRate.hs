module Handler.AddDuesRate where

import Import
import DuesRateForm

getAddDuesRateR :: ClubId -> Handler Html
getAddDuesRateR cid = do 
  (drWidget, enctype) <- generateFormPost $ duesRateForm cid Nothing
  defaultLayout $ [whamlet|
    <h3>Add a dues rate:
    <form method=post enctype=#{enctype}>
      ^{drWidget}
      <input type=submit value=ok>|]

postAddDuesRateR :: ClubId -> Handler Html
postAddDuesRateR cid = do
  ((result, duesRateWidget), enctype) <- runFormPost $ duesRateForm cid Nothing
  case result of 
    FormSuccess duesRate -> do
      runDB $ insert duesRate
      redirect $ DuesRatesR cid
    _ -> error "dues rate problem"  


