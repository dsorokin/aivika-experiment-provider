
-- |
-- Module     : Simulation.Aivika.Experiment.Provider.Utils
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines an utility to return the current real time representation.
--

module Simulation.Aivika.Experiment.Provider.Utils
       (getCurrentLocalTime) where

import Data.Time.LocalTime
import Data.Time.Clock

-- | Get the current local time.
getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime =
  do utc <- getCurrentTime
     timeZone <- getCurrentTimeZone
     let localTime = utcToLocalTime timeZone utc
     return localTime
