module App.StatService (updateStatsService) where

import Happstack.Data
import Happstack.State
import Data.Data (Data)
import System.Log.Logger (Priority(..), logM)
import Network.Beanstalk
import App.State

--updateStatsService :: MVar TxControl -> BeanstalkServer -> IO ()
updateStatsService control bs =
    do st <- statsServer bs
       logM "Happstack.Server" NOTICE "Cron Job Running"
