-----------------------------------------------------------------------------
--
-- Module      :  Blunderbore App StatService
-- Copyright   :  (c) Greg Heartsfield 2010
-- License     :  BSD3
--
-- Scheduled service for updating Beanstalk stats.
--
-----------------------------------------------------------------------------

module App.StatService (updateStatsService) where

import Happstack.Data
import Happstack.State
import Data.Data (Data)
import Control.Concurrent (MVar)
import System.Log.Logger (Priority(..), logM)
import Network.Beanstalk
import App.State
import qualified Data.Map as M
import Data.Maybe(fromJust)

maxStats = 10

updateStatsService :: MVar TxControl -> BeanstalkServer -> IO ()
updateStatsService state bs =
    do st <- statsServer bs
       let ss = ServerStat (getStat "current-jobs-urgent")
                (getStat "current-jobs-ready")
                (getStat "current-jobs-reserved")
                (getStat "current-jobs-delayed")
                (getStat "current-jobs-buried")
                (getStat "total-jobs")
                    where getStat a = read $ fromJust $ M.lookup a st
       update $ AddStat ss M.empty
       stats <- query $ GetStats
       update $ TrimStats maxStats
       putStrLn (show stats)
       logM "Happstack.Server" NOTICE "Cron Job Running"
