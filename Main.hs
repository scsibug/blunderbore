module Main where

import Data.List(intersperse)
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans(liftIO)
import Happstack.Server (nullConf, simpleHTTP, ok, dir, path, ServerPart,
                         toResponse, Response, nullDir)
import Text.XHtml.Transitional hiding (dir)
import Network.Beanstalk

main :: IO ()
main = do bs <- connectBeanstalk "localhost" "11300"
          simpleHTTP nullConf $
                     msum [
                           dir "tube" $ path $ \tube -> (showTubeInfo bs tube)
                           ,dir "tube" (showTubeList bs)
                           ,nullDir >> (showServerStats bs)
                          ]

------------- Tube Info -------------
showTubeInfo :: BeanstalkServer -> String -> ServerPart Response
showTubeInfo bs tube = do tubeInfo <- liftIO $ statsTube bs tube
                          ok $ toResponse $ tubeInfoHtml tubeInfo tube

tubeInfoHtml :: M.Map String String -> String -> Html
tubeInfoHtml stats name = body $ h3 $ stringToHtml
                          ("Tube Stats for "++name) +++ (tableFromMap stats)
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
tubeListHtml tubes = body $ h3 $ stringToHtml "Tubes" +++ (ordList tubeItems)
    where
      tubeItems = map (\name -> hotlink ("/tube/"++name) (stringToHtml name)) tubes
-------------------------------------

-- | Create a table showing key/value pairs
tableFromMap :: M.Map String String -> Html
tableFromMap m = table (concatHtml rows) where
    rows = map (\(k,v) -> tr $ td $ stringToHtml k +++ (td $ stringToHtml v)) kv
    kv = M.assocs m