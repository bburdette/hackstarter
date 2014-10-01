module Handler.Project where

import Import
import qualified Data.Text as T

projectForm :: Project -> Form Project
projectForm proj = renderDivs $ Project
 <$> areq textField "Name" (Just (projectName proj))
 <*> aopt textField "Description" (Just (projectDescription proj))

data LinkTo = LinkTo 
  { toField :: Text }

linkToForm :: Form LinkTo
linkToForm = renderDivs $ LinkTo
  <$> areq textField "project name: " Nothing

-- linkForm :: Form Link
-- linkForm = renderDivs $ Link
--   <$> areq intField "from" Nothing
--   <*> areq intField "to" Nothing 

getProjectR :: ProjectId -> Handler Html
getProjectR projid = do
  mbproj <- runDB $ get projid 
  case mbproj of 
    Nothing -> error $ "invalid project id: " ++ (show projid)
    Just proj -> do 
      (projWidget, enctype) <- generateFormPost $ identifyForm "proj" (projectForm proj)
      (linkWidget, enctype) <- generateFormPost $ identifyForm "link" linkToForm 
      links <- runDB $ selectList [LinkFromproj ==. projid] []
      defaultLayout $ do
        [whamlet|
          <h2>Maintain a record!
          <form method=post enctype=#{enctype}> 
            ^{projWidget}
            <input type=submit value="save changes">
          <hr>
          <h3>Dependencies
          <ul>
            $forall Entity linkid link <- links
              <li> Linked item: #{show (linkFromproj link)}
          <hr>
          <h3>Add dependency
          <form method=post enctype=#{enctype}>
            ^{linkWidget}
            <input type=submit value="add link">
          |]
       
postProjectR :: ProjectId -> Handler Html
postProjectR projid = let dummy = Project "" Nothing in
  do 
    ((resProj, projWidget), enctype) <- runFormPost $ identifyForm "proj" (projectForm dummy)
    ((resLink, linkWidget), enctype) <- runFormPost $ identifyForm "link" linkToForm 
    case resProj of 
      FormSuccess proj -> do 
        result <- runDB $ replace projid proj
        defaultLayout $ [whamlet|
            project update attempted.  result: #{show result}
            <br> <a href=@{ProjectsR}> Back to projects
            |]
      FormFailure errors ->  error $ foldl (++) "there was errors in teh projform! " (map T.unpack errors)
      FormMissing ->
        -- no project form data.  maybe there's link form data?
        case resLink of 
          FormSuccess linktoform -> do
            mbproj <- runDB $ selectFirst [ ProjectName ==. (toField linktoform)] []
            case mbproj of 
              Just (Entity toProjId toproj) ->
                let newlink = Link projid toProjId in do
                  res <- runDB $ insert newlink
                  defaultLayout [whamlet|worked #{show res}|]
              Nothing -> error "baddd"
          FormFailure errors ->  error $ foldr (++) "there was errors in teh linkform! " (map T.unpack errors)
          _ -> error "no form data"
  


