module Handler.TransactionTypes where

import Import

getTransactionTypesR :: Handler Html
getTransactionTypesR = do
  types <- runDB $ selectList [] [Asc TransactionTypeName]
  defaultLayout $ [whamlet|
    <h3> Transaction Types
    <ul>
    $forall Entity tid type <- types
       <li> <a href=@{TransactionTypeR tid}> #{ transactionTypeName type }
    <a href=@{AddTransactionTypeR}> add new transaction type
    |]

postTransactionTypesR :: Handler Html
postTransactionTypesR = error "Not yet implemented: postTransactionTypesR"
