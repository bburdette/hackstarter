module Handler.Club where

import Import
import Permissions
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

data ClubForm = ClubForm
  {
  name :: Text
  }

clubForm :: Maybe Club -> Form Club
clubForm dr = renderDivs $ Club
 <$> areq textField "Club name" (clubName <$> dr) 

data AccountForm = AccountForm
  {
  afName :: Text
  }

accountForm :: Maybe Account -> Form Account
accountForm acc = renderDivs $ Account
 <$> areq textField "add new account:" (accountName <$> acc) 


getClubR :: ClubId -> Handler Html
getClubR cid = do 
  login <- requireAuthId
  requireAdmin login
  mbclub <- runDB $ get cid
  case mbclub of 
    Just _ -> do
      (widge,enc) <- generateFormPost $ identifyForm "club" $ clubForm mbclub
      (widge2,enc2) <- generateFormPost $ identifyForm "account" $ accountForm Nothing
      -- read club accounts, account emails.
      accounts <- runDB $ E.select $ E.from $ \(E.InnerJoin clubaccount account) -> do 
        E.where_ $ clubaccount ^. ClubAccountClub E.==. (E.val cid)
        E.where_ $ clubaccount ^. ClubAccountAccount E.==. account ^. AccountId
        return (clubaccount ^. ClubAccountId, 
                account ^. AccountId,
                account ^. AccountName)
      emails <- runDB $ E.select $ E.from $ \(E.InnerJoin clubemail email) -> do 
        E.where_ $ clubemail ^. ClubEmailClub E.==. (E.val cid)
        E.where_ $ clubemail ^. ClubEmailEmail E.==. email ^. EmailId
        return (clubemail ^. ClubEmailId, 
                email ^. EmailId,
                email ^. EmailEmail)
      defaultLayout $ do
        [whamlet|
          Club Maintenance
          <form method=post enctype=#{enc}> 
            ^{widge}
            <input type=submit value=ok>
          <form method=post enctype=#{enc}>
            <input type=submit name="delete" value="delete">
          <table>
            <th>
              <td> club emails 
            $forall (E.Value ceid, E.Value emailid, E.Value email) <- emails
              <tr>
                <td> #{ email }
          <br>
          <table>
            <th>
              <td> club accounts 
            $forall (E.Value caid, E.Value accountid, E.Value accountname) <- accounts
              <tr>
                <td> #{ accountname }
          <br>
          <form method=post enctype=#{enc}> 
            ^{widge2}
            <input type=submit value=add existing email>
  
        |]
    Nothing -> error $ "club id not found: " ++ show cid
      
postClubR :: ClubId -> Handler Html
postClubR cid = do 
  login <- requireAuthId
  requireAdmin login
  mbDel <- lookupPostParam "delete"
  case mbDel of 
    Just del -> do 
      _ <- runDB $ delete cid
      redirect ClubsR
    Nothing -> do 
      ((res, userWidget),enctype) <- runFormPost $ identifyForm "club" $ clubForm Nothing
      ((res2, widget2),enctype) <- runFormPost $ identifyForm "account" $ accountForm Nothing
      case res of 
        FormSuccess club -> do 
          runDB $ replace cid club
          redirect ClubsR
        FormFailure errs -> error $ "Errors: " ++ show errs 
        -- FormFailure errs -> error $ "Errors: "
        FormMissing -> 
          case res2 of 
            FormSuccess account -> do
              mbaccid <- runDB $ insert $ account 
              runDB $ insert $ ClubAccount cid mbaccid
              redirect $ ClubR cid
            FormFailure errs -> error $ "Errors: " ++ show errs 
            FormMissing -> error $ "form not found."

{-
postDuesRateR :: DuesRateId -> Handler Html
postDuesRateR dri = do
  _ <- requireAuthId
  mbDel <- lookupPostParam "delete"
  case mbDel of 
    Just del -> do 
      _ <- runDB $ do 
        delete dri 
      redirect DuesRatesR
    Nothing -> do 
      ((res,widg),enctype) <- runFormPost (duesRateForm Nothing)
      case res of 
        FormSuccess dr -> do 
          _ <- runDB $ replace dri dr
          redirect DuesRatesR
        _ -> error "bad format error"
-} 
