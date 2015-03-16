module AccountEmailForm where

import Import



data AccountEmailForm = AccountEmailForm
  {
  accountId :: AccountId,
  emailId :: EmailId 
  }

accountEmail :: [(Text, Key Account)] -> [(Text, Key Email)] -> Maybe AccountEmailForm -> Form AccountEmailForm
accountEmail accounts emails caef = renderDivs $ AccountEmailForm
  <$> areq (selectFieldList accounts) "Account" (accountId <$> caef)
  <*> areq (selectFieldList emails) "Emails" (emailId <$> caef)



