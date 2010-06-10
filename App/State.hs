{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances
    #-}
-----------------------------------------------------------------------------
--
-- Module      :  Blunderbore App State
-- Copyright   :  (c) Greg Heartsfield 2010
-- License     :  BSD3
--
-- MACID State for Beanstalk historical stats
--
-----------------------------------------------------------------------------

module App.State where
import Happstack.Data
import Happstack.State
import Happstack.State.ClockTime
import Control.Monad.Reader (ask)
import Control.Monad.State (modify, get, put)
import Data.Data (Data)
import qualified Data.Map as M

data ServerStat = ServerStat { srvUrgentJobs :: Int,
                               srvReadyJobs :: Int,
                               srvReservedJobs :: Int,
                               srvDelayedJobs :: Int,
                               srvBuriedJobs :: Int,
                               srvTotalJobs :: Int
--                             cmdPutCount :: Int,
--                             cmdPeekCount :: Int,
--                             cmdPeekReadyCount :: Int,
--                             cmdPeekDelayedCount :: Int,
--                             cmdPeekBuriedCount :: Int,
--                             cmdReserveCount :: Int,
--                             cmdUseCount :: Int,
--                             cmdWatchCount :: Int,
--                             cmdIgnoreCount :: Int,
--                             cmdDeleteCount :: Int,
--                             cmdReleaseCount :: Int,
--                             cmdBuryCount :: Int,
--                             cmdKickCount :: Int,
--                             cmdStatsCount :: Int,
--                             cmdStatsJobCount :: Int,
--                             cmdStatsTubeCount :: Int,
--                             cmdListTubesCount :: Int,
--                             cmdListTubeUsedCount :: Int,
--                             cmdListTubesWatchedCount :: Int,
--                             cmdPauseTubeCount :: Int,
--                             timeOutCount :: Int,
--                             srvCurrentTubesCount :: Int,
--                             srvCurrentConnections :: Int,
--                             srvCurrentProducers :: Int,
--                             srvCurrentWorkers :: Int,
--                             srvCurrentWaiting :: Int,
--                             srvTotalConnectionsCount :: Int,
--                             srvUtime :: String,
--                             srvStime :: String
                             }
                  deriving (Typeable, Data, Show)

instance Version ServerStat
$(deriveSerialize ''ServerStat)

data TubeStat = TubeStat { tbUrgentJobs :: Int,
                           tbReadyJobs :: Int,
                           tbReservedJobs :: Int,
                           tbDelayedJobs :: Int,
                           tbBuriedJobs :: Int,
                           tbTotalJobs :: Int
                         }
                deriving (Typeable, Data, Show)

instance Version TubeStat
$(deriveSerialize ''TubeStat)

type TubeStats = M.Map String TubeStat

data Stat = Stat { stateHistory :: [(ClockTime,ServerStat,M.Map String TubeStat)] }
            deriving (Typeable, Data)

instance Version Stat
$(deriveSerialize ''Stat)

instance Component Stat where
    type Dependencies Stat = End
    initialValue = Stat []

addStat :: ServerStat -> TubeStats -> Update Stat ()
addStat ss ts = do
  Stat stats <- get
  now <- getEventClockTime
  put (Stat ((now,ss,ts):stats))

getStats :: Query Stat [(ClockTime,ServerStat,TubeStats)]
getStats = do
  Stat s <- ask
  return s

trimStats :: Int -> Update Stat ()
trimStats c = do
  Stat stats <- get
  put (Stat (take c stats))

$(mkMethods ''Stat ['addStat, 'getStats, 'trimStats])