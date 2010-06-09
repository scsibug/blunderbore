{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances
    #-}
-----------------------------------------------------------------------------
--
-- Module      :  Blunderbore App State
-- Copyright   :  (c) Greg Heartsfield 2010
-- License     :  BSD3
--
-- MACID State for Beanstalk historical stats
--
-----------------------------------------------------------------------------

module App.State where
import Happstack.Data
import Happstack.State
import Control.Monad.Reader (ask)
import Control.Monad.State (modify, get, put)
import Data.Data (Data)

data Stat = Stat { readyJobs :: [Int] }
            deriving (Typeable, Data)

instance Version Stat
$(deriveSerialize ''Stat)

instance Component Stat where
    type Dependencies Stat = End
    initialValue = Stat []

addStat :: Int -> Update Stat ()
addStat s = do
  Stat jobstats <- get
  put (Stat (s:jobstats))

getStats :: Query Stat [Int]
getStats  = do
  Stat jobstats <- ask
  return jobstats

$(mkMethods ''Stat ['addStat, 'getStats])