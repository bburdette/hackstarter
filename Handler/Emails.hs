module Handler.Emails where

import Import
import Permissions
import Util

getEmailsR :: Handler Html
getEmailsR = do
  login <- requireAuthId
  requireAdmin login
  emails <- runDB $ selectList [] []
  setSessUrlCurrent "edeller"
  defaultLayout [whamlet|
    <table>
      <tr> 
        <th> 
          <h3> Emails
      $forall (Entity eid eml) <- emails
        <tr>
          <td> 
            #{ emailEmail eml }
          <td>
            <a href=@{DeleteEmailR eid}> delete
    |]

postEmailsR :: Handler Html
postEmailsR = error "Not yet implemented: postEmailsR"
