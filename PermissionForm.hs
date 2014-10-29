module PermissionForm where

import Import

permissionForm :: Maybe Permission -> Form Permission
permissionForm mbperm = renderDivs $ Permission
  <$> areq textField "Name" (permissionName <$> mbperm)
  <*> aopt textField "Description" (permissionDescription <$> mbperm)

