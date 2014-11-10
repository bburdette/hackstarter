module Handler.AddProject where

import Import
import Data.Time

data NewProj = NewProj { 
  name :: Text, 
  description :: Maybe Text 
  } 

newProjectForm :: Form NewProj
newProjectForm = renderDivs $ NewProj
  <$> areq textField "Name" Nothing
  <*> aopt textField "Description" Nothing

getAddProjectR :: Handler Html
getAddProjectR = do
  _ <- requireAuthId
  (projectWidget, enctype) <- generateFormPost newProjectForm
  defaultLayout $ [whamlet|
    <h2>Add a new project!
    <form method=post enctype=#{enctype}>
      ^{projectWidget}
      <input type=submit value="add project">|] 

postAddProjectR :: Handler Html
postAddProjectR = do
  loguid <- requireAuthId
  ((res, projectWidget),enctype) <- runFormPost newProjectForm
  case res of 
    FormSuccess newproj -> do
      now <- lift getCurrentTime
      projID <- runDB $ insert (Project (name newproj) (description newproj) loguid now)
      redirect ProjectsR
    _ -> defaultLayout [whamlet|fale!|]

