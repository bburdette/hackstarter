module Handler.Project where

import Import


projectForm :: Project -> Form Project
projectForm proj = renderDivs $ Project
 <$> areq textField "Name" (Just (projectName proj))
 <*> aopt textField "Description" (Just (projectDescription proj))

getProjectR :: ProjectId -> Handler Html
getProjectR projid = do
  mbproj <- runDB $ get projid 
  case mbproj of 
    Nothing -> error $ "invalid project id: " ++ (show projid)
    Just proj -> do 
      (projWidget, enctype) <- generateFormPost (projectForm proj)
      defaultLayout $ do
        [whamlet|
          <h2>Maintain a record!
          ^{projWidget}
          |]
   
postProjectR :: ProjectId -> Handler Html
postProjectR = error "Not yet implemented: postProjectR"
