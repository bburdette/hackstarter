module Handler.AddDuesRate where

import Import
import DuesRateForm

getAddDuesRateR :: Handler Html
getAddDuesRateR = do 
  (drWidget, enctype) <- generateFormPost $ duesRateForm Nothing
  defaultLayout $ [whamlet|
    <h2>Add a dues rate:
    <form method=post enctype=#{enctype}>
      ^{drWidget}
      <input type=submit value=ok>|]

postAddDuesRateR :: Handler Html
postAddDuesRateR = do
  ((result, duesRateWidget), enctype) <- runFormPost $ duesRateForm Nothing
  case result of 
    FormSuccess duesRate -> do
      runDB $ insert duesRate
      redirect DuesRatesR
    _ -> error "dues rate problem"  


