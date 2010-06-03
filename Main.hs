module Main where

import Data.List(intersperse)
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans(liftIO)
import Happstack.Server (nullConf, simpleHTTP, ok, dir, path, ServerPart, toResponse, Response)
import Text.XHtml.Transitional hiding (dir)
import Network.Beanstalk

main :: IO ()
main = do bs <- connectBeanstalk "localhost" "11300"
          simpleHTTP nullConf $ msum [
                                     dir "tube" $ path $ \tube -> (showTubeInfo bs tube)
                                     ,dir "tube" (showTubeList bs)
                                     ,showServerStats bs -- default
                                     ]

------------- Tube Info -------------
showTubeInfo :: BeanstalkServer -> String -> ServerPart Response
showTubeInfo bs tube = do tubeInfo <- liftIO (statsTube bs tube)
                          ok $ toResponse $ tubeInfoHtml tubeInfo tube

tubeInfoHtml :: M.Map String String -> String -> Html
tubeInfoHtml stats name = body (concatHtml [h3 (stringToHtml ("Tube Stats for "++name)),statTable])
    where
      statTable = table (concatHtml rows)
      rows = map (\(k,v) -> tr (concatHtml [(td (stringToHtml k)), (td (stringToHtml v))])) kv
      kv = M.assocs stats
-------------------------------------

------------- Server Stats ----------
showServerStats :: BeanstalkServer -> ServerPart Response
showServerStats bs = do stats <- liftIO (statsServer bs)
                        ok $ toResponse $ statsHtml stats

statsHtml :: M.Map String String -> Html
statsHtml stats = body (concatHtml [h3 (stringToHtml "Server Stats"),statTable])
    where
      statTable = table (concatHtml rows)
      rows = map (\(k,v) -> tr (concatHtml [(td (stringToHtml k)), (td (stringToHtml v))])) kv
      kv = M.assocs stats
-------------------------------------

------------- Tube List -------------
showTubeList :: BeanstalkServer -> ServerPart Response
showTubeList bs = do tubes <- liftIO (listTubes bs)
                     ok $ toResponse $ tubeListHtml tubes

tubeListHtml :: [String] -> Html
tubeListHtml tubes = body (concatHtml [h3 (stringToHtml "Tubes"),tubeList])
    where
      tubeList = ordList tubeItems
      tubeItems = map (\name -> hotlink ("/tube/"++name) (stringToHtml name)) tubes
-------------------------------------
