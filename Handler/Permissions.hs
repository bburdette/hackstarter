module Handler.Permissions where

import Import

permissionForm :: Maybe Permission -> Form Permission
permissionForm mbperm = renderDivs $ Permission
  <$> areq textField "Name" (permissionName <$> mbperm)
  <*> aopt textField "Description" (permissionDescription <$> mbperm)

getPermissionsR :: Handler Html
getPermissionsR = do
  perms <- runDB $ selectList [] []
  (perform, enctype) <- generateFormPost $ permissionForm Nothing
  defaultLayout $ [whamlet|
    <table>
      <tr>
        <th> Permissions
      $forall (Entity pid perm) <- perms
        <tr>
          <td> #{ permissionName perm }
    <h4>add permission
    <form method=post enctype=#{enctype}>
      ^{perform}
      <input type=submit value=add>
    |]

postPermissionsR :: Handler Html
postPermissionsR = do
  ((res, permWidget),enctype) <- runFormPost $ permissionForm Nothing
  case res of 
    FormSuccess perm -> do
      permID <- runDB $ insert perm
      redirect PermissionsR
    _ -> defaultLayout [whamlet|fale!|]

