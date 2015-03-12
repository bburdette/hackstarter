module Handler.AddClub where

import Import
import Permissions 

data ClubForm = ClubForm
  {
  name :: Text
  }

addClubForm :: Form ClubForm
addClubForm = renderDivs $ ClubForm
 <$> areq textField "Club name:" Nothing

getAddClubR :: Handler Html
getAddClubR = do 
  login <- requireAuthId
  requireAdmin login
  (formWidge, enctype) <- generateFormPost addClubForm
  defaultLayout $ do 
    [whamlet|
      Add new club
      <form method=post enctype=#{enctype}>
        ^{formWidge}
        <input type=submit value=add>
      |] 

postAddClubR :: Handler Html
postAddClubR = do
  login <- requireAuthId
  requireAdmin login
  ((res, userWidget),enctype) <- runFormPost addClubForm 
  case res of
    FormSuccess club -> do
      _ <- runDB $ insert $ Club (name club)
      redirect $ ClubsR


