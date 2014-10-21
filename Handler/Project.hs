module Handler.Project where

import Import
import qualified Data.Text as T
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

projectForm :: Project -> Form Project
projectForm proj = renderDivs $ Project
 <$> areq textField "Name" (Just (projectName proj))
 <*> aopt textField "Description" (Just (projectDescription proj))

data LinkTo = LinkTo 
  { toField :: Text }

linkToForm :: Form LinkTo
linkToForm = renderDivs $ LinkTo
  <$> areq textField "project name: " Nothing

getProjectR :: ProjectId -> Handler Html
getProjectR projid = do
  mbproj <- runDB $ get projid 
  case mbproj of 
    Nothing -> error $ "invalid project id: " ++ (show projid)
    Just proj -> do 
      (projWidget, enctype) <- generateFormPost $ identifyForm "proj" (projectForm proj)
      (linkWidget, enctype) <- generateFormPost $ identifyForm "link" linkToForm 
      namedlinks <- runDB $ E.select 
        $ E.from $ \(E.InnerJoin link project) -> do 
          E.on $ link ^. LinkFromproj E.==. (E.val projid) 
            E.&&. project ^. ProjectId E.==. link ^. LinkToproj 
          return 
            ( link ^. LinkToproj, project ^. ProjectName ) 
      defaultLayout $ do
        [whamlet|
          <h2>Maintain a record!
          <form method=post enctype=#{enctype}> 
            ^{projWidget}
            <input type=submit name=save value="save changes">
            <input type=submit name=delete value="delete project">
          <hr>
          <h3>Dependencies
          <ul>
          $forall (E.Value projkey, E.Value projname) <- namedlinks
            <li> 
              <a href=@{ProjectR projkey}> #{projname} 
              blah
          <hr>
          <h3>Add dependency
          <form method=post enctype=#{enctype}>
            ^{linkWidget}
            <input type=submit value="add link">
          |]
       
postProjectR :: ProjectId -> Handler Html
postProjectR projid = let dummy = Project "" Nothing in
  do 
    ((resProj, projWidget), enctype) <- 
        runFormPost $ identifyForm "proj" (projectForm dummy)
    ((resLink, linkWidget), enctype) <- 
        runFormPost $ identifyForm "link" linkToForm 
    case resProj of 
      FormSuccess proj -> do
        del <- lookupPostParam "delete"
        sav <- lookupPostParam "save"
        case (del, sav) of 
          (Just _, Nothing) -> do 
            result <- runDB $ delete projid
            defaultLayout $ [whamlet|
                project delete attempted.  result: #{show result}
                <br> <a href=@{ProjectsR}> Back to projects
                |]
          (Nothing, Just _) -> do
            result <- runDB $ replace projid proj
            defaultLayout $ [whamlet|
                project update attempted.  result: #{show result}
                <br> <a href=@{ProjectsR}> Back to projects
                |]
          _ -> error "unknown button name!"
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
  


