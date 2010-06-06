module App.Controller (appHandler) where

import Data.List(intersperse)
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans(liftIO)
import Happstack.Server
    (Conf(port), nullConf, simpleHTTP, ok, dir, path, ServerPart,
     toResponse, Response, nullDir)
import Text.XHtml.Transitional hiding (dir)
import Network.Beanstalk

appHandler :: BeanstalkServer -> ServerPart Response
appHandler bs = msum [
                 dir "job" $ path $ \job -> (showJobInfo bs job)
                ,dir "tube" $ path $ \tube -> (showTubeInfo bs tube)
                ,dir "tube" (showTubeList bs)
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
