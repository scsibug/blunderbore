module App.StatService (updateStatsService) where

import Happstack.Data
import Happstack.State
import Data.Data (Data)
import System.Log.Logger (Priority(..), logM)
import Network.Beanstalk
import App.State

--updateStatsService :: MVar TxControl -> BeanstalkServer -> IO ()
updateStatsService state bs =
    do st <- statsServer bs
       update $ AddStat 4
       stats <- query $ GetStats
       putStrLn (show stats)
       logM "Happstack.Server" NOTICE "Cron Job Running"
