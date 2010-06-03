module Main where

import Control.Monad
import Happstack.Server (nullConf, simpleHTTP, ok, dir, path)

main :: IO ()
main = simpleHTTP nullConf $ msum [dir "tube" $ path $ \tubename -> ok $ "Tube "++tubename
                                  ,dir "tube" $ ok "Hello World!"
                                  ,ok "Root or error"
                                  ]