module Handler.DeleteUser where

import Import hiding ((=.), (==.), (!=.), (||.), delete, update)
import Database.Esqueleto
import qualified Database.Persist as P
import Permissions
import Util

{-
getDeleteEmailR :: EmailId -> Handler Html
getDeleteEmailR eid = do 
  login <- requireAuthId
  requireAdmin login
  runDB $ P.delete eid
  redirectSessUrl dellerSessKey HomeR

    update $ (\email -> do
      set email [ EmailUser =. nothing ]
      where_ (email ^. EmailUser ==. (just (val uid))))
-}

getDeleteUserR :: UserId -> Handler Html
getDeleteUserR uid = do
  -- get user accounts.
  vaccts <- runDB $ select $ from (\useracct -> do
    where_ $ useracct ^. UserAccountUser ==. (val uid)
    return $ useracct ^. UserAccountAccount)
  let accts = ((\(Value a) -> a) <$> vaccts) 
  -- for each account, how many owners?
  owners <- mapM getAccountOwners accts
  let ocount = length <$> owners
      delacts = filter (\(_,b) -> (b < 2)) (zip accts ocount)
  -- first try deleting the user.  might fail because lots of 
  -- userid foreign keys.
  runDB $ do
    update $ (\email -> do
      set email [ EmailUser =. nothing ]
      where_ (email ^. EmailUser ==. (just (val uid))))
    delete $ from (\uacct -> do 
      where_ $ uacct ^. UserAccountUser ==. val uid)
    delete $ from (\user -> do 
      where_ $ user ^. UserId ==. val uid) 
  -- delete the accounts.
  mapM deleteAccount $ fst <$> delacts
  -- we'll delete the accounts with single owners. 
  defaultLayout $ do 
    [whamlet|
      <br> #{ show accts }
      <br> #{ show owners }
      <br> #{ show delacts }
      |]
      
  {-
  if only user on accounts, delete accounts.
  to delete accounts delete from:
    useraccount   
    paypal->internal and 
    internal
    bankinternal
    accountemail
    account
  also remove owner from email records.
    userpermissions
  -}

deleteAccount :: AccountId -> Handler ()
deleteAccount aid = do
  runDB $ do 
    delete $ from (\uacct -> do 
      where_ $ uacct ^. UserAccountAccount ==. val aid)
    delete $ from (\cacct -> do 
      where_ $ cacct ^. ClubAccountAccount ==. val aid)
    delete $ from (\pacct -> do 
      where_ $ pacct ^. ProjectAccountAccount ==. val aid)
    delete $ from (\ppi -> do 
      where_ $ ppi ^. PaypalInternalToaccount ==. val aid)
    delete $ from (\bi -> do 
      where_ $ bi ^. BankInternalToaccount ==. val aid)
    delete $ from (\i -> do 
      where_ $ 
        (i ^. InternalToaccount ==. val aid) &&.
        (i ^. InternalFromaccount ==. val aid))
    update $ (\club -> do
      set club [ ClubDuesaccount =. nothing ]
      where_ (club ^. ClubDuesaccount ==. (just (val aid))))
    delete $ from (\ae -> do
      where_ $ ae ^. AccountEmailAccount ==. val aid)
    delete $ from (\acct -> do
      where_ (acct ^. AccountId ==. val aid)) 
  return ()
    
{-
deleteAccount :: AccountId -> Handler ()
deleteAccount aid = do
  runDB $ delete $ from (\uacct -> do where_ $ uacct ^. UserAccountAccount ==. val aid)
  runDB $ delete $ from (\cacct -> do where_ $ cacct ^. ClubAccountAccount ==. val aid)
  runDB $ delete $ from (\pacct -> do where_ $ pacct ^. ProjectAccountAccount ==. val aid)
  return ()
-}


getAccountOwners :: AccountId -> Handler [Text]
getAccountOwners aid = do
  clubs <- runDB $ select $ from (\(InnerJoin club clubaccount) -> do
    on $ club ^. ClubId ==. clubaccount ^. ClubAccountClub
    where_ $ clubaccount ^. ClubAccountAccount ==. (val aid)
    return $ club ^. ClubName)
  users <- runDB $ select $ from (\(InnerJoin user useraccount) -> do
    on $ user ^. UserId ==. useraccount ^. UserAccountUser
    where_ $ useraccount ^. UserAccountAccount ==. (val aid)
    return $ user ^. UserIdent)
  return $ ((\(Value a) -> a) <$> users) ++ ((\(Value a) -> a) <$> clubs)

 
