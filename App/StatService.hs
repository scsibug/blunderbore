-----------------------------------------------------------------------------
--
-- Module      :  Blunderbore App StatService
-- Copyright   :  (c) Greg Heartsfield 2010
-- License     :  BSD3
--
-- Scheduled service for updating Beanstalk stats.
--
-----------------------------------------------------------------------------

module App.StatService (updateStatsService,trimStatsService) where

import Happstack.Data
import Happstack.State
import Data.Data (Data)
import System.Log.Logger (infoM,noticeM)
import Network.Beanstalk
import App.State
import qualified Data.Map as M
import Data.Maybe(fromJust)

maxStats = 120

updateStatsService :: BeanstalkServer -> IO ()
updateStatsService bs =
    do st <- statsServer bs
       let ss = ServerStat (getStat "current-jobs-urgent")
                (getStat "current-jobs-ready")
                (getStat "current-jobs-reserved")
                (getStat "current-jobs-delayed")
                (getStat "current-jobs-buried")
                (getStat "total-jobs")
                    where getStat a = read $ fromJust $ M.lookup a st
       stats <- query $ GetStats
       infoM "Happstack.Server" ("Stat count: "++(show (length stats)))
       update $ AddStat ss M.empty
       infoM "Happstack.Server" "Stats updated"

trimStatsService :: IO ()
trimStatsService =
    do update $ TrimStats maxStats
       infoM "Happstack.Server" "Removed old stats"