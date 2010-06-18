module App.Controller (appHandler) where

import Data.List(intercalate)
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans(liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.State (modify, get, put)
import Happstack.State
import Happstack.Server
    (Conf(port), nullConf, simpleHTTP, ok, dir, path, ServerPart,
     toResponse, Response, nullDir)
import Happstack.Server.HTTP.FileServe(fileServeStrict)
import Text.XHtml.Transitional hiding (dir)
import Data.List(intersperse)
--import Data.Yaml.Syck
import Text.JSON
import System.Time
import System.Locale
import Network.Beanstalk
import App.State

appHandler :: BeanstalkServer -> ServerPart Response
appHandler bs = msum [
                 dir "job" $ path $ \job -> (showJobInfo bs job)
                ,dir "tube" $ path $ \tube -> (showTubeInfo bs tube)
                ,dir "tube" (showTubeList bs)
                ,dir "server" $ dir "stats" $ dir "csv" $ showServerStatsCSV
                ,dir "server" $ dir "stats" $ dir "json" $ showServerStatsJson
                ,dir "static" $ fileServeStrict [] "static"
                ,nullDir >> (showServerStats bs)
                ]

------------- Job Info --------------
showJobInfo :: BeanstalkServer -> Int -> ServerPart Response
showJobInfo bs job = do jobInfo <- liftIO $ statsJob bs job
                        ok $ toResponse $ jobInfoHtml jobInfo (show job)

jobInfoHtml :: M.Map String String -> String -> Html
jobInfoHtml stats name = body $ (h3 $ stringToHtml ("Stats for Job #"++name))
                          +++ (tableFromMap stats)
-------------------------------------

------------- Tube Info -------------
showTubeInfo :: BeanstalkServer -> String -> ServerPart Response
showTubeInfo bs tube = do tubeInfo <- liftIO $ statsTube bs tube
                          ok $ toResponse $ tubeInfoHtml tubeInfo tube

tubeInfoHtml :: M.Map String String -> String -> Html
tubeInfoHtml stats name = body $ (h3 $ stringToHtml ("Tube Stats for "++name))
                          +++ (tableFromMap stats)
-------------------------------------

------------- Server Stats ----------
showServerStats :: BeanstalkServer -> ServerPart Response
showServerStats bs = do stats <- liftIO $ statsServer bs
                        ok $ toResponse $ statsHtml stats

statsHtml :: M.Map String String -> Html
statsHtml stats = body (h3 $ stringToHtml "Server Stats") +++ (tableFromMap stats)

showServerStatsCSV :: ServerPart Response
showServerStatsCSV =
    do stats <- query $ GetStats
       let header = "utc_date,js_timestamp, urgent_jobs, ready_jobs, reserved_jobs, delayed_jobs, buried_jobs, total_jobs\n"
       let statsrows = map (\(x,y,z) -> (utcFromClockTime x)++","++(show $ ctToJSTS x)++","++(serverstatline y)) stats
               where serverstatline a = intercalate "," $ map (\x -> show(x a)) [srvUrgentJobs,srvReadyJobs,srvReservedJobs,srvDelayedJobs,srvBuriedJobs,srvTotalJobs]
       return $ toResponse $ header++(unlines statsrows)

showServerStatsJson :: ServerPart Response
showServerStatsJson =
    do stats <- query $ GetStats
       let times = map (\(x,_,_) -> JSRational False $ toRational $ ctToJSTS x) stats
       let filterJobs jobf = map (\(_,y,_) -> JSRational False $ toRational $ jobf y) stats
       let makeSeries jobtype = map(\(t,j) -> JSArray [t,j]) (zip times (filterJobs jobtype))
       let seriesWithLabel label series = showJSON $ toJSObject [("label",showJSON (toJSString label)),("data",series)]
       let series = JSArray [
                     seriesWithLabel "Urgent" (JSArray (makeSeries srvUrgentJobs)),
                     seriesWithLabel "Ready" (JSArray (makeSeries srvReadyJobs)),
                     seriesWithLabel "Reserved" (JSArray (makeSeries srvReservedJobs)),
                     seriesWithLabel "Delayed" (JSArray (makeSeries srvDelayedJobs)),
                     seriesWithLabel "Buried" (JSArray (makeSeries srvBuriedJobs))
                    ]

       return $ toResponse $ encode series


-- | ClockTime to a javascript timestamp (milliseconds since Jan 1 1970)
ctToJSTS :: ClockTime -> Integer
ctToJSTS (TOD sec _) = sec * 1000

utcFromClockTime :: ClockTime -> String
utcFromClockTime c = calendarTimeToString utc_time
    where utc_time = (toUTCTime c) {ctTZName = "GMT"}

-------------------------------------

------------- Tube List -------------
showTubeList :: BeanstalkServer -> ServerPart Response
showTubeList bs = do tubes <- liftIO $ listTubes bs
                     ok $ toResponse $ tubeListHtml tubes

tubeListHtml :: [String] -> Html
tubeListHtml tubes = body $ (h3 $ stringToHtml "Tubes") +++ (ordList tubeItems)
    where
      tubeItems = map (\name -> hotlink ("/tube/"++name) (stringToHtml name)) tubes
-------------------------------------

-- | Create a table showing key/value pairs
tableFromMap :: M.Map String String -> Html
tableFromMap m = table (concatHtml rows) where
    rows = map (\(k,v) -> tr $ td $ stringToHtml k +++ (td $ stringToHtml v)) kv
    kv = M.assocs m
