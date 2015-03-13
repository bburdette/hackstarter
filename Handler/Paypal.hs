module Handler.Paypal where

import Import
import Permissions
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import Data.Fixed

getPaypalR :: Handler Html
getPaypalR = do
  logid <- requireAuthId
  requireAdmin logid
  mahsums <- runDB $ E.select 
    $ E.from $ \lolwut -> do 
      let sumg = (E.sum_ (lolwut ^. PaypalAmountGross))
          sumn = (E.sum_ (lolwut ^. PaypalAmountNet))
      return (sumg, sumn)
  let (sumg, sumn) = case mahsums of 
                [(E.Value (Just gamt), E.Value (Just namt))] -> (gamt, namt)
                _ -> (0,0) :: (Centi, Centi)
   in do
    ledges <- runDB $ E.select 
      $ E.from $ \(E.InnerJoin (E.LeftOuterJoin (E.LeftOuterJoin paypal email) email2) usercreator) -> do 
        E.where_ $ usercreator ^. UserId E.==. paypal ^. PaypalCreator
        E.where_ $ (paypal ^. PaypalFromemail E.==. email E.?. EmailId) 
        E.where_ $ (paypal ^. PaypalToemail E.==. email2 E.?. EmailId) 
        E.orderBy $ [E.asc ( paypal ^. PaypalDate)]
        return 
          ( paypal ^. PaypalDate,
            paypal ^. PaypalAmountGross,
            paypal ^. PaypalAmountNet,
            paypal ^. PaypalName,
            paypal ^. PaypalCreator,
            paypal ^. PaypalDescription,
            paypal ^. PaypalMemo,
            email E.?. EmailEmail,
            email2 E.?. EmailEmail,
            usercreator ^. UserIdent ) 
    defaultLayout $ do 
      [whamlet| 
        <h4> Paypal
        <br> Sum of transactions: #{show sumg} #{show sumn}
        <table class="ledgarrr">
          <tr>
            <th> Datetime
            <th> Gross
            <th> Net
            <th> Name
            <th> Description
            <th> Memo
            <th> From Email 
            <th> To Email 
            <th> Creator
          $forall (E.Value datetime, E.Value gamount, E.Value namount, E.Value name, E.Value creator, E.Value description, E.Value memo, E.Value from_emailtxt, E.Value to_emailtxt, E.Value creatorIdent) <- ledges
            <tr>
              <td> #{ show datetime}
              <td> #{ show gamount }
              <td> #{ show namount }
              <td> #{ name } 
              <td> #{ description }
              <td> #{ memo }
              <td> #{ maybe "" id from_emailtxt }
              <td> #{ maybe "" id to_emailtxt }
              <td> #{ creatorIdent }
      |]

postPaypalR :: Handler Html
postPaypalR = error "Not yet implemented: postPaypalR"
