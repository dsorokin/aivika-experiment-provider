
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.IO.Provider
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module allows using 'IO' to provide simulation data results.
--

module Simulation.Aivika.Experiment.IO.Provider () where

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Experiment
import Simulation.Aivika.Experiment.Trans.Provider.Types

import Simulation.Aivika.IO

-- | An instance of 'ExperimentMonadProviding'.
instance ExperimentMonadProviding ExperimentProvider IO where

  -- | This is a synonym for the 'IO' monad.
  type ExperimentMonad ExperimentProvider IO = IO

-- | An instance of 'ExperimentProviding'.
instance ExperimentProviding ExperimentProvider IO
