module AccountEmailForm where

import Import

accountForm :: Maybe Account -> Form Account
accountForm acc = renderDivs $ Account
 <$> areq textField "add new account:" (accountName <$> acc) 
 <*> areq hiddenField "ignorethis" (accountClub <$> acc) 

data AccountEmailForm = AccountEmailForm
  {
  accountId :: AccountId,
  emailId :: EmailId 
  }

accountEmailForm :: [(Text, Key Account)] -> [(Text, Key Email)] -> Maybe AccountEmailForm -> Form AccountEmailForm
accountEmailForm accounts emails caef = renderDivs $ AccountEmailForm
  <$> areq (selectFieldList accounts) "Account" (accountId <$> caef)
  <*> areq (selectFieldList emails) "Emails" (emailId <$> caef)



