module Handler.Clubs where

import Import
import Permissions
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

getClubsR :: Handler Html
getClubsR = do
  logid <- requireAuthId
  requireAdmin logid
  rows <- runDB $ E.select $ E.from $ \clubs -> do
    E.orderBy $ [E.asc ( clubs ^. ClubName) ]
    return ( clubs ^. ClubId,
             clubs ^. ClubName )  
  defaultLayout $ do 
    [whamlet|
      <table class="clubtable">
        <tr>
          <th> Club Name
        $forall (E.Value id, E.Value name) <- rows
          <tr>
            <td> 
              <a href=@{ClubR id }> #{ name }
      <a href=@{AddClubR}> new club
      |]

postClubsR :: Handler Html
postClubsR = error "Not yet implemented: postClubsR"
