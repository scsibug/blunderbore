module Main where

import Data.List(intersperse)
import Control.Monad
import Control.Monad.Trans(liftIO)
import Happstack.Server (nullConf, simpleHTTP, ok, dir, path)
import Network.Beanstalk

main :: IO ()
main = do bs <- connectBeanstalk "localhost" "11300"
          simpleHTTP nullConf $ msum [dir "tube" $ path $ \tubename -> ok $ "Tube "++tubename
                                     ,dir "tube" (tubeListStr bs)
                                     ,ok "Use /tube/tubename"
                                     ]


tubeListStr bs = do lt <- liftIO (listTubes bs)
                    ok (foldr (++) "" (intersperse "\n" lt))
