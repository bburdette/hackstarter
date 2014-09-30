module Handler.Project where

import Import


projectForm :: Project -> Form Project
projectForm proj = renderDivs $ Project
 <$> areq textField "Name" (Just (projectName proj))
 <*> aopt textField "Description" (Just (projectDescription proj))

-- linkForm :: Form Link
-- linkForm = renderDivs $ Link
--    <$> aref  

getProjectR :: ProjectId -> Handler Html
getProjectR projid = do
  mbproj <- runDB $ get projid 
  case mbproj of 
    Nothing -> error $ "invalid project id: " ++ (show projid)
    Just proj -> do 
      (projWidget, enctype) <- generateFormPost (projectForm proj)
      links <- runDB $ selectList [] []
      defaultLayout $ do
        [whamlet|
          <h2>Maintain a record!
          <form method=post enctype=#{enctype}> 
            ^{projWidget}
            <input type=submit value="save changes">
          <h3>Dependencies
          <ul>
            $forall Entity linkid link <- links
              <li> Linked item: #{show (linkFromproj link)}
          |]
       
postProjectR :: ProjectId -> Handler Html
postProjectR projid = let dummy = Project "" Nothing in
  do 
    ((res, projWidget), enctype) <- runFormPost (projectForm dummy)
    case res of 
      FormSuccess proj -> do 
        result <- runDB $ replace projid proj
        defaultLayout $ [whamlet|
            project update attempted.  result: #{show result}
            <br> <a href=@{ProjectsR}> Back to projects
            |]
      _ -> error "there was an error"


