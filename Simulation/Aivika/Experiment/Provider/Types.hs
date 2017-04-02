
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Provider.Types
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines an experiment provider of simulation results.
--

module Simulation.Aivika.Experiment.Provider.Types
       (ExperimentProvider(..),
        ExperimentProviderEnvironment(..),
        makeExperimentProviderContext,
        contextExperimentEnvironment,
        contextSourceId) where

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Entity

-- | The experiment provider.
data ExperimentProvider =
  ExperimentProvider { providerExperimentAggregator :: ExperimentAggregator
                       -- ^ The experiment aggregator.
                     }

-- | The experiment provider environment.
data ExperimentProviderEnvironment =
  ExperimentProviderEnvironment { environmentExperimentProvider :: ExperimentProvider,
                                  -- ^ The experiment provider.
                                  environmentExperiment :: Experiment,
                                  -- ^ The experiment.
                                  environmentExperimentId :: UUID
                                  -- ^ The experiment identifier.
                                }

-- | Rending with the results when running the simulation experiment.
instance ExperimentRendering ExperimentProvider where

  -- | The experiment context.
  data ExperimentContext ExperimentProvider =
    ExperimentProviderContext { contextExperimentEnvironment :: ExperimentProviderEnvironment,
                                -- ^ The experiment environment.
                                contextSourceId :: SourceUUID
                                -- ^ The source identifier.
                              }

  -- | The experiment environment.
  type ExperimentEnvironment ExperimentProvider = ExperimentProviderEnvironment

  -- | The experiment rendering monad.
  type ExperimentMonad ExperimentProvider = IO

  liftExperiment r = id

  prepareExperiment e provider = undefined

  renderExperiment e r reporters env = undefined

-- | Make the experiment context.
makeExperimentProviderContext :: ExperimentProviderEnvironment -> SourceUUID -> ExperimentContext ExperimentProvider
makeExperimentProviderContext = ExperimentProviderContext
