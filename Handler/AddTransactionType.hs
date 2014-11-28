module Handler.AddTransactionType where

import Import

addTransactionTypeForm :: Form TransactionType
addTransactionTypeForm = renderDivs $ TransactionType
  <$> areq textField "Transaction Type name" Nothing 

getAddTransactionTypeR :: Handler Html
getAddTransactionTypeR = do 
  (widget,enctype) <- generateFormPost addTransactionTypeForm
  defaultLayout $ [whamlet|
    <h3> add transaction type
    <form method=post enctype=#{enctype}>
      ^{widget}
      <input type=submit value=ok>
    |]

postAddTransactionTypeR :: Handler Html
postAddTransactionTypeR = do 
  ((res, widget),enctype) <- runFormPost addTransactionTypeForm
  case res of
    FormSuccess ttype -> do
      _  <- runDB $ insert ttype 
      redirect TransactionTypesR  
    _ -> defaultLayout [whamlet|fale|]    
