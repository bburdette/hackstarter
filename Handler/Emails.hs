module Handler.Emails where

import Import
import Permissions

getEmailsR :: Handler Html
getEmailsR = do
  login <- requireAuthId
  requireAdmin login
  emails <- runDB $ selectList [] []
  defaultLayout [whamlet|
    <table>
      <tr> 
        <th> 
          <h3> Emails
      $forall (Entity eid eml) <- emails
        <tr>
          <td> 
            #{ emailEmail eml }
    |]

postEmailsR :: Handler Html
postEmailsR = error "Not yet implemented: postEmailsR"
