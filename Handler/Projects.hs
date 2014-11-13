module Handler.Projects where

import Import

getProjectsR :: Handler Html
getProjectsR = do
  _ <- requireAuthId
  projs <- runDB $ selectList [] [] 
  defaultLayout $ [whamlet|
    <h3> _{MsgHackerSpace} Projects:
    <ul>
    $forall Entity projId proj <- projs
      <li> <a href=@{ProjectR projId}> #{projectName proj}
    <br>
    <a href=@{AddProjectR}>add new project
    |]

postProjectsR :: Handler Html
postProjectsR = error "Not yet implemented: postProjectsR"
