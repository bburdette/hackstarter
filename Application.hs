{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Database.Persist
import Database.Persist.Sql (runMigration)
import Network.HTTP.Client.Conduit (newManager)
import Control.Monad.Logger (runLoggingT)
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import Network.Wai.Logger (clockDateCacher)
import Data.Default (def)
import Yesod.Core.Types (loggerSet, Logger (Logger))

-- BTB for sqlite foreign key activation. 
-- import ForkeyOpen

import Permissions

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home
import Handler.Users
import Handler.AddUser
import Handler.Projects
import Handler.AddProject
import Handler.Project
import Handler.User
import Handler.DuesRates
import Handler.AddDuesRate
import Handler.DuesRate
import Handler.AddLedgerEntry
import Handler.Ledger
import Handler.Permissions
import Handler.Permission
import Handler.LoginTest
import Handler.UserPermissionDelete
import Handler.AddPermission
import Handler.Utilities
import Handler.Emails
import Handler.AddEmail
import Handler.Paypal
import Handler.Bank
import Handler.Clubs
import Handler.AddClub
import Handler.Club
import Handler.ClubEmailRemove
import Handler.ClubAccountDelete
import Handler.AccountEmailRemove
import Handler.CreatePaypalMembers
import Handler.Account
import Handler.AccountDues
import Handler.AddInternal
import Handler.DeleteInternal
import Handler.PaypalDetail
import Handler.EditInternal
import Handler.DeleteEmail
import Handler.DeleteUser

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO (Application, LogFunc)
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    let logFunc = messageLoggerSource foundation (appLogger foundation)
    return (logWare $ defaultMiddlewaresNoLogging app, logFunc)

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager
    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    -- p <- forKeyCreatePoolConfig (dbconf :: Settings.PersistConf)
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)

    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher

    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App conf s p manager dbconf logger

    -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)
        (messageLoggerSource foundation logger)

    -- execute this Handler for the sqls
    _ <- runFakeHandler mempty appLogger foundation checkAdmin

    return foundation

{-
-- doesn't compile for me, but maybe try to make it work later.
fakeHandler :: App -> Handler a -> IO a
fakeHandler app f = 
    runFakeHandler mempty appLogger app f >>= either
        (errorM . ("runFakeHandler issue: " <>) . show)
        return
-}

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader (fmap fst . makeApplication)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
