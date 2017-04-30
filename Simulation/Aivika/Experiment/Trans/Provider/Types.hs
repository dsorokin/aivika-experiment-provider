
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
        ExperimentProviding(..),
        makeExperimentProviderContext,
        contextExperimentEnvironment) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Trans.Experiment
import Simulation.Aivika.Experiment.Provider.Utils

-- | The experiment provider.
data ExperimentProvider =
  ExperimentProvider { providerExperimentAggregator :: ExperimentAggregator,
                       -- ^ The experiment aggregator.
                       providerExperimentId :: Maybe ExperimentUUID
                       -- ^ The optional experiment identifier.
                     }

-- | The experiment provider environment.
data ExperimentProviderEnvironment m =
  ExperimentProviderEnvironment { environmentExperimentProvider :: ExperimentProvider,
                                  -- ^ The experiment provider.
                                  environmentExperiment :: Experiment m,
                                  -- ^ The experiment.
                                  environmentExperimentEntity :: ExperimentEntity,
                                  -- ^ The experiment entity.
                                  environmentExperimentId :: UUID
                                  -- ^ The experiment identifier.
                                }

-- | Rending with the results when running the simulation experiment.
instance (ExperimentMonadProviding ExperimentProvider m,
          ExperimentProviding ExperimentProvider m)
         => ExperimentRendering ExperimentProvider m where

  -- | The experiment context.
  data ExperimentContext ExperimentProvider m =
    ExperimentProviderContext { contextExperimentEnvironment :: ExperimentProviderEnvironment m
                                -- ^ The experiment environment.
                              }

  -- | The experiment environment.
  type ExperimentEnvironment ExperimentProvider m = ExperimentProviderEnvironment m

  prepareExperiment e provider =
    liftIO $
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
                                            experimentEntityRealStartTime = show localTime,
                                            experimentEntityCompleted = False,
                                            experimentEntityErrorMessage = Nothing }
                   f <- tryWriteExperimentEntity agent expEntity
                   case f of
                     True  -> return (Just expEntity)
                     False -> return Nothing
       let expId = experimentEntityId expEntity
       return ExperimentProviderEnvironment { environmentExperimentProvider = provider,
                                              environmentExperiment = e,
                                              environmentExperimentEntity = expEntity,
                                              environmentExperimentId = expId }

  renderExperiment e provider reporters env = return ()

  onExperimentCompleted e provider env =
    liftIO $
    do let aggregator = providerExperimentAggregator provider
           agent      = experimentAggregatorAgent aggregator
           expEntity  = environmentExperimentEntity env
       updateExperimentEntity agent $
         expEntity { experimentEntityCompleted = True }

  onExperimentFailed e provider env e' =
    liftIO $
    do let aggregator = providerExperimentAggregator provider
           agent      = experimentAggregatorAgent aggregator
           expEntity  = environmentExperimentEntity env
       updateExperimentEntity agent $
         expEntity { experimentEntityErrorMessage = Just (show e') }

-- | Make the experiment context.
makeExperimentProviderContext :: ExperimentMonadProviding ExperimentProvider m
                                 => ExperimentProviderEnvironment m
                                 -> ExperimentContext ExperimentProvider m
{-# INLINABLE makeExperimentProviderContext #-}
makeExperimentProviderContext = ExperimentProviderContext

-- | Defines the constraints when the experiment can be provided.
class (MonadDES m,
       MonadIO (Event m),
       EventIOQueueing m,
       ExperimentMonadProviding r m,
       Monad (ExperimentMonad r m),
       MonadIO (ExperimentMonad r m)) => ExperimentProviding r m
