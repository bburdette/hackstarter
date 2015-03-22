module Handler.CreatePaypalMembers where

import Import
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

{-

  show (name, email) pairs
    that are in paypal transactions
      that are TO an email that belongs to the club
    that are NOT in account_email already
    show only one of each

  does account need to belong to a club as well?  
    I think yes.  
      you could derive the origin of money in the account by looking at the paypal
      transaction, but that's a pain.  want to do it with just the paypalinternal. 
    This expresses the idea that money is given to a club.  
    One can only have transactions from account to account within a club.  
  

-}

data UserMake = UserMake
  {
    paypals :: [PaypalId] 
  }

userMakeForm :: [(Text,PaypalId)] -> Maybe UserMake -> Form UserMake
userMakeForm choisez mbum = renderDivs $ UserMake 
  <$> areq (checkboxesFieldList choisez) "valid users?" (paypals <$> mbum)

{-
  restoo <- runDB $ E.select $ E.from $ 
    (\(E.LeftOuterJoin paypal email) -> do
      E.where_ $ paypal ^. PaypalFromemail E.==. email E.?. EmailId
      E.where_ $ E.notExists $ 
        E.from $ \accountemail -> do
          E.where_ (paypal ^. PaypalFromemail E.==. 
                    accountemail E.?. AccountEmailEmail)
      E.where_ $ E.in_ (paypal E.^. PaypalToemail) $ 
        E.select $ E.from $ \(E.InnerJoin clubaccount aeml) -> do 
          E.where_ $ clubaccount ^. ClubAccountClub E.==. (E.val cid)
          E.where_ $ clubaccount ^. ClubAccountAccount E.==. aeml ^. AccountEmailAccount
          return (aeml ^. AccountEmailEmail)
      return (paypal ^. PaypalName, paypal ^. PaypalId, email E.?. EmailEmail)) 
 
  restoo <- runDB $ E.select $ E.from $ 
    (\(E.LeftOuterJoin paypal email) -> do
      E.where_ $ paypal ^. PaypalFromemail E.==. email E.?. EmailId
      E.where_ $ E.notExists $ 
        E.from $ \accountemail -> do
          E.where_ (paypal ^. PaypalFromemail E.==. 
                    accountemail E.?. AccountEmailEmail)
      E.where_ $ E.exists $ 
        E.from $ \(E.InnerJoin clubaccount aeml) -> do 
          E.where_ $ clubaccount ^. ClubAccountClub E.==. (E.val cid)
          E.where_ $ clubaccount ^. ClubAccountAccount E.==. aeml ^. AccountEmailAccount
          E.where_ (paypal ^. PaypalToemail E.==. 
                    aeml ^. AccountEmailEmail)
      return (paypal ^. PaypalName, paypal ^. PaypalId, email E.?. EmailEmail)) 
 -}
getCreatePaypalMembersR :: ClubId -> Handler Html
--getCreatePaypalMembersR cid = error "blah"

getCreatePaypalMembersR cid = do  
  restoo <- runDB $ E.select $ E.from $ 
    (\(E.LeftOuterJoin paypal email) -> do
      E.where_ $ paypal ^. PaypalFromemail E.==. email E.?. EmailId
      E.where_ $ E.notExists $ 
        E.from $ \accountemail -> do
          E.where_ (paypal ^. PaypalFromemail E.==. 
                    accountemail E.?. AccountEmailEmail)
      E.where_ $ E.exists $ 
        E.from $ \aeml -> do 
          E.where_ (paypal ^. PaypalToemail E.==. 
                    aeml E.?. AccountEmailEmail)
          E.where_ $ E.in_ (aeml E.?. AccountEmailAccount) $
            E.subList_select $ E.from $ \clubaccount -> do
              E.where_ $ clubaccount ^. ClubAccountClub E.==. (E.val cid)
              return $ E.just $ clubaccount ^. ClubAccountAccount
      return (paypal ^. PaypalName, paypal ^. PaypalId, email E.?. EmailEmail)) 
  let chex = (\(E.Value name, E.Value ppid, E.Value email) -> 
               (name, ppid)) <$> restoo
      ppids = (\(_, E.Value ppid, _) -> 
               ppid) <$> restoo
  (umform, umenc) <- generateFormPost $ userMakeForm chex (Just $ UserMake ppids)
  defaultLayout $ do [whamlet|
    cppuresult:
    <form method=post enctype=#{ umenc }>
      ^{umform}
      <input type=submit value=save>
    <table>
      <tr>
        <th> ppn
        <th> pid
        <th> email
      $forall (E.Value ppn, E.Value pid, eml) <- restoo
        <tr>
          <td> #{ ppn }
          <td> #{ show pid }
          $case eml 
            $of (E.Value (Just email))
              <td> #{ email } 
            $of (E.Value Nothing)
              <td> 
   |]

{-

select email.email from account_email, club_account, email where club_account.club == 1 and club_account.account == account_email.account and email.id == account_email.email;

-}


{-
FormFailure err -> error $ show err
FormMissing -> case cpptresult of 
  FormSuccess meh -> do 
    defaultLayout $ do [whamlet|
      cpptresult:
      <br> #{ (mehVal meh) }
    |]
  FormFailure err -> error $ show err
  FormMissing -> error "form missing"             
-}

postCreatePaypalMembersR :: ClubId -> Handler Html
postCreatePaypalMembersR = error "Not yet implemented: postCreatePaypalMembersR"
