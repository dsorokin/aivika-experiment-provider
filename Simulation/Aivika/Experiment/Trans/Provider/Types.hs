
{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Trans.Provider.Types
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines an experiment provider of simulation results.
--

module Simulation.Aivika.Experiment.Trans.Provider.Types
       (ExperimentProvider(..),
        ExperimentProviderEnvironment(..),
        makeExperimentProviderContext,
        contextExperimentEnvironment,
        contextSourceId) where

import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Trans.Experiment

-- | The experiment provider.
data ExperimentProvider =
  ExperimentProvider { providerExperimentAggregator :: ExperimentAggregator
                       -- ^ The experiment aggregator.
                     }

-- | The experiment provider environment.
data ExperimentProviderEnvironment m =
  ExperimentProviderEnvironment { environmentExperimentProvider :: ExperimentProvider,
                                  -- ^ The experiment provider.
                                  environmentExperiment :: Experiment m,
                                  -- ^ The experiment.
                                  environmentExperimentId :: UUID
                                  -- ^ The experiment identifier.
                                }

-- | Rending with the results when running the simulation experiment.
instance ExperimentMonadProviding ExperimentProvider m => ExperimentRendering ExperimentProvider m where

  -- | The experiment context.
  data ExperimentContext ExperimentProvider m =
    ExperimentProviderContext { contextExperimentEnvironment :: ExperimentProviderEnvironment m,
                                -- ^ The experiment environment.
                                contextSourceId :: SourceUUID
                                -- ^ The source identifier.
                              }

  -- | The experiment environment.
  type ExperimentEnvironment ExperimentProvider m = ExperimentProviderEnvironment m

  prepareExperiment e provider = undefined

  renderExperiment e r reporters env = undefined

-- | Make the experiment context.
makeExperimentProviderContext :: ExperimentMonadProviding ExperimentProvider m
                                 => ExperimentProviderEnvironment m
                                 -> SourceUUID
                                 -> ExperimentContext ExperimentProvider m
{-# INLINABLE makeExperimentProviderContext #-}
makeExperimentProviderContext = ExperimentProviderContext
