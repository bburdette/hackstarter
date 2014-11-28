module Handler.TransactionType where

import Import

editTransactionType :: TransactionType -> Form TransactionType
editTransactionType ttype = renderDivs $ TransactionType
  <$> areq textField "Transaction Type name" (Just (transactionTypeName ttype))

getTransactionTypeR :: TransactionTypeId -> Handler Html
getTransactionTypeR ttid = do 
  ttype <- runDB $ get404 ttid
  (widget, enctype) <- generateFormPost $ editTransactionType ttype
  defaultLayout $ [whamlet|
    <h3> Transaction Type
    <form method=post enctype=#{enctype}>
      ^{widget}
      <input type=submit value=ok>
    |]      
   

postTransactionTypeR :: TransactionTypeId -> Handler Html
postTransactionTypeR = error "Not yet implemented: postTransactionTypeR"


