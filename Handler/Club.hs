module Handler.Club where

import Import
import Permissions
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import AccountEmailForm

data ClubForm = ClubForm
  {
  name :: Text
  }

clubForm :: Maybe Club -> Form Club
clubForm dr = renderDivs $ Club
 <$> areq textField "Club name" (clubName <$> dr) 

data EmailForm = EmailForm
  {
  emName :: Text
  }

emailForm :: Maybe EmailForm -> Form EmailForm
emailForm emlf = renderDivs $ EmailForm
 <$> areq textField "add new Email:" (emName <$> emlf) 


getClubR :: ClubId -> Handler Html
getClubR cid = do 
  login <- requireAuthId
  requireAdmin login
  mbclub <- runDB $ get cid
  case mbclub of 
    Just _ -> do
      (widge,enc) <- generateFormPost $ identifyForm "club" $ clubForm mbclub
      (awidge,aenc) <- generateFormPost $ identifyForm "account" $ accountForm Nothing
      (ewidge,eenc) <- generateFormPost $ identifyForm "email" $ emailForm Nothing
      -- read club accounts, account emails.
      accounts <- runDB $ E.select $ E.from $ \(E.InnerJoin clubaccount account) -> do 
        E.on ((clubaccount ^. ClubAccountClub E.==. (E.val cid)) E.&&.
              (clubaccount ^. ClubAccountAccount E.==. account ^. AccountId))
        return (clubaccount ^. ClubAccountId, 
                account ^. AccountId,
                account ^. AccountName)
      emails <- runDB $ E.select $ E.from $ \(E.InnerJoin clubemail email) -> do 
        E.on ((clubemail ^. ClubEmailClub E.==. (E.val cid)) E.&&.
              (clubemail ^. ClubEmailEmail E.==. email ^. EmailId))
        return (clubemail ^. ClubEmailId, 
                email ^. EmailId,
                email ^. EmailEmail)
      accountemails <- runDB $ E.select $ E.from $ 
        \(E.InnerJoin (E.InnerJoin clubaccount accountemail) email) -> do
          E.on $ accountemail ^. AccountEmailEmail  E.==. email ^. EmailId
          E.on ((clubaccount ^. ClubAccountClub E.==. (E.val cid)) E.&&.
                (clubaccount ^. ClubAccountAccount E.==. accountemail ^. AccountEmailAccount))
          return (clubaccount ^. ClubAccountAccount, 
                  accountemail ^. AccountEmailId,
                  email ^. EmailId, 
                  email ^. EmailEmail)
      let accountEmailGrid = foldl (addacct accountemails) [] accounts
          addacct accemls lst (clubAccountId, accountId, accountName) = 
            let emls = filter (\(acct,_,_,_) -> acct == accountId) accemls
                news = (accountName, Just clubAccountId, E.Value "", Nothing) : 
                  (fmap (\(_,acctemlid,_,emltxt) -> (E.Value "", Nothing, emltxt, Just acctemlid)) emls) in 
              lst ++ news
      (cewidge,ceenc) <- generateFormPost $ identifyForm "accountemail" $ 
        accountEmail (fmap (\(_,E.Value accid,E.Value acctxt) -> (acctxt, accid)) accounts) (fmap (\(_,E.Value emlid,E.Value emltxt) -> (emltxt, emlid)) emails) Nothing
      defaultLayout $ do
        [whamlet|
          <h4> Club Maintenance
          <form method=post enctype=#{enc}> 
            ^{widge}
            <input type=submit value=ok>
          <form method=post enctype=#{enc}>
            <input type=submit name="delete" value="delete">
          <table>
            <tr>
              <th> club emails 
            $forall (E.Value ceid, E.Value emailid, E.Value email) <- emails
              <tr>
                <td> #{ email }
                <td> 
                  <a href="#" onClick="post('@{ClubEmailRemoveR ceid}', {})"> remove
          <br>
          <form method=post enctype=#{eenc}> 
            ^{ewidge}
            <input type=submit value=add existing email>
          <table>
            <tr>
              <th> account 
              <th> email
            $forall (E.Value account, mbclubacctid, E.Value email, mbaccountemailid) <- accountEmailGrid
              <tr>
                <td> #{ account }
                <td> #{ email }
                <td>
                  $case mbclubacctid 
                    $of (Just (E.Value clubacctid))
                      <td> 
                        <a href="#" onClick="post('@{ClubAccountDeleteR clubacctid}', {})"> delete
                    $of Nothing
                      $case mbaccountemailid 
                        $of (Just (E.Value accountemailid)) 
                          <td> 
                            <a href="#" onClick="post('@{AccountEmailRemoveR accountemailid}', {})"> remove
                        $of Nothing
                            
          <br>
          <form method=post enctype=#{aenc}> 
            ^{awidge}
            <input type=submit value=add account>
          <form method=post enctype=#{aenc}> 
            ^{cewidge}
            <input type=submit value=add email>
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
      ((c_res, _),_) <- runFormPost $ identifyForm "club" $ clubForm Nothing
      ((a_res, _),_) <- runFormPost $ identifyForm "account" $ accountForm Nothing
      ((e_res, _),_) <- runFormPost $ identifyForm "email" $ emailForm Nothing
      accounts <- runDB $ E.select $ E.from $ \(E.InnerJoin clubaccount account) -> do 
        E.on ((clubaccount ^. ClubAccountClub E.==. (E.val cid)) E.&&.
              (clubaccount ^. ClubAccountAccount E.==. account ^. AccountId))
        return (clubaccount ^. ClubAccountId, 
                account ^. AccountId,
                account ^. AccountName)
      emails <- runDB $ E.select $ E.from $ \(E.InnerJoin clubemail email) -> do 
        E.on ((clubemail ^. ClubEmailClub E.==. (E.val cid)) E.&&.
              (clubemail ^. ClubEmailEmail E.==. email ^. EmailId))
        return (clubemail ^. ClubEmailId, 
                email ^. EmailId,
                email ^. EmailEmail)
      ((ce_res,_),_) <- runFormPost $ identifyForm "accountemail" $ 
        accountEmail (fmap (\(_,E.Value accid,E.Value acctxt) -> (acctxt, accid)) accounts) (fmap (\(_,E.Value emlid,E.Value emltxt) -> (emltxt, emlid)) emails) Nothing
      -- ((ce_res,_),_) <- runFormPost $ identifyForm "accountemail" $  clubAccountEmail [] [] Nothing
      case c_res of 
        FormSuccess club -> do 
          runDB $ replace cid club
          redirect ClubsR
        FormFailure errs -> error $ "Errors: " ++ show errs 
        -- FormFailure errs -> error $ "Errors: "
        FormMissing -> 
          case a_res of 
            FormSuccess account -> do
              mbaccid <- runDB $ insert $ account 
              runDB $ insert $ ClubAccount cid mbaccid
              redirect $ ClubR cid
            FormFailure errs -> error $ "Errors: " ++ show errs 
            FormMissing -> do
              case e_res of  
                FormSuccess emlform -> do
                  -- does this email exist?
                  mbemail <- runDB $ selectFirst [EmailEmail ==. (emName emlform)] []
                  case mbemail of 
                    Nothing -> error "email not found: " 
                    Just (Entity emid eml) -> do
                      runDB $ insert $ ClubEmail cid emid
                      redirect $ ClubR cid
                FormFailure errs -> error $ "Errors: " ++ show errs 
                FormMissing -> 
                  case ce_res of  
                    FormSuccess accemlform -> do
                      wut <- runDB $ insert $ AccountEmail (accountId accemlform) (emailId accemlform)
                      redirect $ ClubR cid
                    FormFailure errs -> error $ "Errors: " ++ show errs 
                    FormMissing -> error "no form" 

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
