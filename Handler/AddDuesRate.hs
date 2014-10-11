module Handler.AddDuesRate where

import Import

newDuesRateForm :: Form DuesRate
newDuesRateForm = renderDivs $ DuesRate
 <$> areq textField "Dues rate name" Nothing
 <*> areq intField "Dues rate amount" Nothing 

getAddDuesRateR :: Handler Html
getAddDuesRateR = do 
  (drWidget, enctype) <- generateFormPost newDuesRateForm
  defaultLayout $ [whamlet|
    <h2>Add a dues rate:
    <form method=post enctype=#{enctype}>
      ^{drWidget}
      <input type=submit value=ok>|]

postAddDuesRateR :: Handler Html
postAddDuesRateR = do
  ((result, duesRateWidget), enctype) <- runFormPost newDuesRateForm
  case result of 
    FormSuccess duesRate -> do
      runDB $ insert duesRate
      redirect DuesRatesR
    _ -> error "dues rate problem"  


