module Handler.Bank where

import Import
import Permissions
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import Data.Fixed

getBankR :: Handler Html
getBankR = do
  logid <- requireAuthId
  requireAdmin logid
  rows <- runDB $ E.select $ E.from $ \bankses -> do
    E.orderBy $ [E.asc ( bankses ^. BankDate), 
                 E.asc ( bankses ^. BankTransactionid)]
    return 
      (bankses ^. BankTransactionid,
       bankses ^. BankDate,
       bankses ^. BankDescription,
       bankses ^. BankMemo,
       bankses ^. BankAmount,
       bankses ^. BankCheckNumber)
  defaultLayout $ do 
    [whamlet| 
      <h4> Bank
      <table class="ledgarrr">
        <tr>
          <th> Date
          <th> Amount 
          <th> Description
          <th> Memo
          <th> Check Number 
          <th> Transaction ID
        $forall (E.Value transid,E.Value date,E.Value desc,E.Value memo,E.Value amount,E.Value checkno) <- rows 
          <tr>
            <td> #{ show date}
            <td> #{ show amount }
            <td> #{ desc }
            <td> #{ memo }
            <td> #{ maybe "" id (fmap show checkno) }
            <td> #{ transid } 
    |]

 
   

postBankR :: Handler Html
postBankR = error "Not yet implemented: postBankR"
