
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
        contextExperimentEnvironment) where

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Provider.Utils

-- | The experiment provider.
data ExperimentProvider =
  ExperimentProvider { providerExperimentAggregator :: ExperimentAggregator,
                       -- ^ The experiment aggregator.
                       providerExperimentId :: Maybe ExperimentUUID
                       -- ^ The optional experiment identifier.
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
    ExperimentProviderContext { contextExperimentEnvironment :: ExperimentProviderEnvironment
                                -- ^ The experiment environment.
                              }

  -- | The experiment environment.
  type ExperimentEnvironment ExperimentProvider = ExperimentProviderEnvironment

  -- | The experiment rendering monad.
  type ExperimentMonad ExperimentProvider = IO

  liftExperiment r = id

  prepareExperiment e provider =
    do let aggregator = providerExperimentAggregator provider
           agent      = experimentAggregatorAgent aggregator
       initialiseEntitySchema agent
       expEntity <-
         retryAgentAction agent $
         do expId <-
              case providerExperimentId provider of
                Nothing -> newRandomUUID
                Just x  -> return x
            expEntity <- readExperimentEntity agent expId
            case expEntity of
              Just expEntity -> return (Just expEntity)
              Nothing ->
                do localTime <- getCurrentLocalTime
                   let integMethod = 
                         case spcMethod (experimentSpecs e) of
                           Euler       -> EulerIntegMethod
                           RungeKutta2 -> RK2IntegMethod
                           RungeKutta4 -> RK4IntegMethod
                       expEntity =
                         ExperimentEntity { experimentEntityId = expId,
                                            experimentEntityTitle = experimentTitle e,
                                            experimentEntityDescription = experimentDescription e,
                                            experimentEntityStartTime = spcStartTime (experimentSpecs e),
                                            experimentEntityStopTime = spcStopTime (experimentSpecs e),
                                            experimentEntityDT = spcDT (experimentSpecs e),
                                            experimentEntityIntegMethod = integMethod,
                                            experimentEntityRunCount = experimentRunCount e,
                                            experimentEntityRealStartTime = show localTime }
                   f <- tryWriteExperimentEntity agent expEntity
                   case f of
                     True  -> return (Just expEntity)
                     False -> return Nothing
       let expId = experimentEntityId expEntity
       return ExperimentProviderEnvironment { environmentExperimentProvider = provider,
                                              environmentExperiment = e,
                                              environmentExperimentId = expId }

  renderExperiment e provider reporters env = return ()

-- | Make the experiment context.
makeExperimentProviderContext :: ExperimentProviderEnvironment -> ExperimentContext ExperimentProvider
makeExperimentProviderContext = ExperimentProviderContext
