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
import Happstack.State.ClockTime
import Control.Monad.Reader (ask)
import Control.Monad.State (modify, get, put)
import Data.Data (Data)

data Stat = Stat { readyJobs :: [(ClockTime,Int)] }
            deriving (Typeable, Data)

instance Version Stat
$(deriveSerialize ''Stat)

instance Component Stat where
    type Dependencies Stat = End
    initialValue = Stat []

addStat :: Int -> Update Stat ()
addStat s = do
  Stat jobstats <- get
  now <- getEventClockTime
  put (Stat ((now,s):jobstats))

getStats :: Query Stat [(ClockTime,Int)]
getStats  = do
  Stat jobstats <- ask
  return jobstats

$(mkMethods ''Stat ['addStat, 'getStats])