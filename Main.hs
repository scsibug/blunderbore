-----------------------------------------------------------------------------
--
-- Module      :  Blunderbore Main
-- Copyright   :  (c) Greg Heartsfield 2010
-- License     :  BSD3
--
-- This is the main executable for the Happstack-powered Blunderbore,
-- a web-based Beanstalk monitoring application.
--
-- Contains lots of code from the Happstack guestbook example.
-----------------------------------------------------------------------------

module Main where

import Data.List(intersperse)
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans(liftIO)
import Control.Concurrent (MVar, forkIO, killThread)
import Control.Exception (bracket)
import Happstack.Util.Cron (cron)
import Happstack.Server
    (Conf(port), nullConf, simpleHTTP, ok, dir, path, ServerPart,
     toResponse, Response, nullDir, validator, wdgHTMLValidator
    )
import Happstack.State
  ( Component
  , Proxy(..)
  , Methods
  , TxControl
  , Saver(Queue, FileSaver)
  , runTxSystem
  , shutdownSystem
  , createCheckpoint
  )
import Happstack.State (waitForTermination)

import Network.Beanstalk

import System.Exit (exitWith, ExitCode(..), exitFailure)
import System.Environment (getArgs)
import System.Log.Logger (Priority(..), logM)
import System.Console.GetOpt
import App.Logger (withLogger)
import App.State (Stat)
import App.StatService (updateStatsService)
import App.Controller (appHandler)

------------------------------------------------------------------------------
-- Server
------------------------------------------------------------------------------

-- | Configuration information
data AppConf
    = AppConf { httpConf :: Conf
              , store :: FilePath
              , static :: FilePath
              }

-- | Default configuration
defaultConf :: String -> AppConf
defaultConf progName
    = AppConf { httpConf = nullConf
              , store    = "_local/" ++ progName ++ "_state"
              , static   = "public"
              }

main :: IO ()
main = withLogger $ do
         flags <- parseConfig =<< getArgs
         displayInfo flags
         runServer flags

runServer :: [Flag] -> IO ()
runServer flags = do
  let appConf = foldr ($) (defaultConf progName) [f | ServerConfig f <- flags]
  bs <- connectBeanstalk "localhost" "11300"
  -- Start state system
  withSystemState' (store appConf) stateProxy $ \control -> do
    -- Get server stats
    withThread (cron 1 (updateStatsService control bs)) $ do
      logM "Happstack.Server" NOTICE "Starting cron service"
    -- Checkpoint server state once a day
      withThread (cron (60*60*24) (createCheckpoint control)) $ do
        logM "Happstack.Server" NOTICE "Starting checkpoint service"
        -- Start HTTP server
        withThread (simpleHTTP (httpConf appConf) (appHandler bs)) $ do
          logM "Happstack.Server" NOTICE "System running, press Ctrl-C to stop server"
          waitForTermination
  where
  startSystemState' :: (Component st, Methods st) => String -> Proxy st -> IO (MVar TxControl)
  startSystemState' = runTxSystem . Queue . FileSaver

  withSystemState' :: (Component st, Methods st) => String -> Proxy st -> (MVar TxControl -> IO a) -> IO a
  withSystemState' name proxy action =
    bracket (startSystemState' name proxy) createCheckpointAndShutdown action

  withThread init action = bracket (forkIO $ init) (killThread) (\_ -> action)

  createCheckpointAndShutdown control = do
    logM "Happstack.Server" NOTICE "Creating system checkpoint"
    createCheckpoint control
    logM "Happstack.Server" NOTICE "System shutdown"
    shutdownSystem control

  stateProxy :: Proxy Stat
  stateProxy = Proxy

-- | Display information required by flags. Currently, this is either the help
-- message or the version information.
displayInfo :: [Flag] -> IO ()
displayInfo flags
  | any isHelp    flags = putStr helpMessage >> exitWith ExitSuccess
  | any isVersion flags = putStr versionInfo >> exitWith ExitSuccess
  | otherwise           = return ()

------------------------------------------------------------------------------
-- Command Line Interface Content
------------------------------------------------------------------------------

-- | Program name used to identify your application.
-- It influences the version and help message, as well as the directory name
-- for the state of your application.
progName = "blunderbore"

fullName = "Blunderbore"

copyrightInfo = "Copyright (C) 2010 Greg Heartsfield"
licenceInfo = "BSD3"

-- | Version information
versionInfo = unlines
  [ fullName ++ " (" ++ progName ++ ")"
  , copyrightInfo
  , licenceInfo
  ]

-- | A simple usage message listing all flags possible.
helpMessage = usageInfo header opts
  where
  header = "Usage: "++progName++" [OPTION...]"

------------------------------------------------------------------------------
-- Command Line Parsing
------------------------------------------------------------------------------

-- | Flags
data Flag =
    ServerConfig (AppConf -> AppConf)
  | Help
  | Version

-- Flag selectors
isHelp    flag = case flag of Help    -> True; _ -> False
isVersion flag = case flag of Version -> True; _ -> False

-- | Command line options.
opts :: [OptDescr Flag]
opts = [ Option [] ["http-port"]   (ReqArg setPort "port")                        "Port to bind http server"
       , Option [] ["no-validate"] (NoArg $ setValidator Nothing)                 "Turn off HTML validation"
       , Option [] ["validate"]    (NoArg $ setValidator (Just wdgHTMLValidator)) "Turn on HTML validation"
       , Option [] ["store"]       (ReqArg setMacidDir "PATH")                    "The directory used for database storage."
       , Option [] ["static"]      (ReqArg setStaticDir "PATH")                   "The directory searched for static files"
       , Option [] ["version"]     (NoArg Version)                                "Display version information"
       , Option [] ["help"]        (NoArg Help)                                   "Display this help message"
       ]
     where
     setPort p      = ServerConfig $ \c -> c { httpConf = (httpConf c) {port      = read p} }
     setValidator v = ServerConfig $ \c -> c { httpConf = (httpConf c) {validator = v     } }
     setMacidDir p  = ServerConfig $ \c -> c { store = p }
     setStaticDir p = ServerConfig $ \c -> c { static = p }

-- | Parse the command line arguments into a list of flags. Exits with usage
-- message, in case of failure.
parseConfig :: [String] -> IO [Flag]
parseConfig args = case getOpt Permute opts args of
  (flags,_,[]) -> return flags
  (_,_,errs)   -> do
    logM progName ERROR ("Failure while parsing command line:\n"++unlines errs)
    putStr helpMessage
    exitFailure