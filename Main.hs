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
          simpleHTTP nullConf $ msum [--dir "tube" $ path $ \tubename
--                                          -> (tubeInfoStr bs tubename)
--                                     ,dir "tube" (tubeListStr bs)
                                     showServerStats bs --serverStatStr bs
                                     ]

showServerStats :: BeanstalkServer -> ServerPart Response
showServerStats bs = do stats <- liftIO (statsServer bs)
                        ok $ toResponse $ statsHtml stats

statsHtml :: M.Map String String -> Html
statsHtml stats = body (concatHtml [h3 (stringToHtml "Server Stats"),statTable])
    where
      statTable = table (concatHtml rows)
      rows = map (\(k,v) -> tr (concatHtml [(td (stringToHtml k)), (td (stringToHtml v))])) kv
--      statStr = (foldr (++) "" (intersperse "\n" statslist))
--      statslist = map (\(k,v) -> (k ++ " => " ++ v)) kv
      kv = M.assocs stats

serverStatStr bs = do stats <- liftIO (statsServer bs)
                      let kv = M.assocs stats
                      let statslist = map (\(k,v) -> (k ++ " => " ++ v)) kv
                      ok (foldr (++) "" (intersperse "\n" statslist))

tubeListStr bs = do lt <- liftIO (listTubes bs)
                    ok (foldr (++) "" (intersperse "\n" lt))

tubeInfoStr bs tubename = do stats <- liftIO (statsTube bs tubename)
                             let kv = M.assocs stats
                             let statslist = map (\(k,v) -> (k ++ " => " ++ v)) kv
                             ok (foldr (++) "" (intersperse "\n" statslist))

