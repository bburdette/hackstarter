module AccountEmailForm where

import Import

accountForm :: [(Text,ClubId)] -> Maybe Account -> Form Account
accountForm clubs mbacc = renderDivs $ Account
 <$> areq textField "add new account:" (accountName <$> mbacc) 
 <*> areq (selectFieldList clubs) "in club" (accountClub <$> mbacc) 

accountWClubForm :: ClubId -> Form Account
accountWClubForm cid = renderDivs $ Account
 <$> areq textField "add new account:" Nothing
 <*> areq hiddenField "" (Just cid)

data AccountEmailForm = AccountEmailForm
  {
  accountId :: AccountId,
  emailId :: EmailId 
  }

accountEmailForm :: [(Text, Key Account)] -> [(Text, Key Email)] -> Maybe AccountEmailForm -> Form AccountEmailForm
accountEmailForm accounts emails caef = renderDivs $ AccountEmailForm
  <$> areq (selectFieldList accounts) "Account" (accountId <$> caef)
  <*> areq (selectFieldList emails) "Emails" (emailId <$> caef)



