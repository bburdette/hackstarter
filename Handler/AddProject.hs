module Handler.AddProject where

import Import

newProjectForm :: Form Project
newProjectForm = renderDivs $ Project
 <$> areq textField "Name" Nothing
 <*> aopt textField "Description" Nothing

getAddProjectR :: Handler Html
getAddProjectR = do
  (projectWidget, enctype) <- generateFormPost newProjectForm
  defaultLayout $ [whamlet|
    <h2>Add a new project!
    <form method=post enctype=#{enctype}>
      ^{projectWidget}
      <input type=submit value="add project">|] 

postAddProjectR :: Handler Html
postAddProjectR = do
  ((res, projectWidget),enctype) <- runFormPost newProjectForm
  case res of 
    FormSuccess proj -> do
      projID <- runDB $ insert proj
      redirect ProjectsR
    _ -> defaultLayout [whamlet|fale!|]

