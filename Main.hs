module Main where

import Data.List(intersperse)
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans(liftIO)
import Happstack.Server (nullConf, simpleHTTP, ok, dir, path)
import Network.Beanstalk

main :: IO ()
main = do bs <- connectBeanstalk "localhost" "11300"
          simpleHTTP nullConf $ msum [dir "tube" $ path $ \tubename
                                          -> (tubeInfoStr bs tubename)
                                     ,dir "tube" (tubeListStr bs)
                                     ,ok "Use /tube/tubename"
                                     ]


tubeListStr bs = do lt <- liftIO (listTubes bs)
                    ok (foldr (++) "" (intersperse "\n" lt))

tubeInfoStr bs tubename = do stats <- liftIO (statsTube bs tubename)
                             let kv = M.assocs stats
                             let statslist = map (\(k,v) -> (k ++ " => " ++ v)) kv
                             ok (foldr (++) "" (intersperse "\n" statslist))
