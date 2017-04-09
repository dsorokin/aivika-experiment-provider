
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Trans.Provider.FinalSamplingStatsView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'FinalSamplingStatsView' that provides with
-- sample-based statistics data in final time points.
--

module Simulation.Aivika.Experiment.Trans.Provider.FinalSamplingStatsView 
       (FinalSamplingStatsView(..), 
        defaultFinalSamplingStatsView) where

import Control.Monad
import Control.Monad.Trans

import Data.Array

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Experiment
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Trans.Provider.Types

-- | Defines the 'View' that provides with the sample-based statistics data
-- in final time points.
data FinalSamplingStatsView m =
  FinalSamplingStatsView { finalSamplingStatsKey :: String,
                           -- ^ The source key.
                           finalSamplingStatsTitle :: String,
                           -- ^ The title.
                           finalSamplingStatsDescription :: String,
                           -- ^ The description.
                           finalSamplingStatsTransform :: ResultTransform m,
                           -- ^ The transform applied to the results before receiving series.
                           finalSamplingStatsSeries :: ResultTransform m
                           -- ^ It defines the series to provide with.
                         }
  
-- | The default 'FinalSamplingStatsView'.  
defaultFinalSamplingStatsView :: MonadDES m => FinalSamplingStatsView m
{-# INLINABLE defaultFinalSamplingStatsView #-}
defaultFinalSamplingStatsView = 
  FinalSamplingStatsView { finalSamplingStatsKey       = error "Provide with the finalSamplingStatsKey field value",
                           finalSamplingStatsTitle     = "Final Sample-based Statistics",
                           finalSamplingStatsDescription = "",
                           finalSamplingStatsTransform = id,
                           finalSamplingStatsSeries    = id }
  
instance ExperimentProviding ExperimentProvider m => ExperimentView (FinalSamplingStatsView m) ExperimentProvider m where

  {-# INLINABLE outputView #-}
  outputView v = 
    let reporter exp provider env =
          let ctx = makeExperimentProviderContext env
          in return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateView v ctx,
                                         reporterContext    = ctx }
    in ExperimentGenerator { generateReporter = reporter }
       
-- | Provide with the simulation results.
simulateView :: ExperimentProviding ExperimentProvider m
                => FinalSamplingStatsView m
                -> ExperimentContext ExperimentProvider m
                -> ExperimentData m
                -> Composite m ()
{-# INLINABLE simulateView #-}
simulateView view ctx expdata =
  do let rs      = finalSamplingStatsSeries view $
                   finalSamplingStatsTransform view $
                   experimentResults expdata
         exts    = resultsToDoubleStatsValues rs
         signals = experimentPredefinedSignals expdata
         signal  = resultSignalInStopTime signals
         srcKey  = finalSamplingStatsKey view
         title   = finalSamplingStatsTitle view
         descr   = finalSamplingStatsDescription view
         env        = contextExperimentEnvironment ctx
         provider   = environmentExperimentProvider env
         aggregator = providerExperimentAggregator provider
         agent      = experimentAggregatorAgent aggregator
         exp        = environmentExperiment env
         expId      = environmentExperimentId env
         loc        = experimentLocalisation exp
     i  <- liftParameter simulationIndex
     disposableComposite $
       DisposableEvent $
       do ns <- forM exts $ \ext ->
            return (resultValueName ext, loc $ resultValueId ext)
          srcEntity <- liftIO $ readOrCreateSourceEntityByKey agent expId srcKey title descr ns FinalSamplingStatsEntityType
          let vars  = sourceEntityVarEntities srcEntity
              srcId = sourceEntityId srcEntity
          entities <-
            forM (zip vars exts) $ \(var, ext) ->
            do t <- liftDynamics time
               n <- liftDynamics integIteration
               a <- resultValueData ext
               entityId <- liftIO newRandomUUID
               let item   = DataItem { dataItemValue = a,
                                       dataItemIteration = n,
                                       dataItemTime = t }
                   entity = DataEntity { dataEntityId = entityId,
                                         dataEntityExperimentId = expId,
                                         dataEntityRunIndex = i,
                                         dataEntityVarId = varEntityId var,
                                         dataEntitySourceId = srcId,
                                         dataEntityItem = item }
               return entity
          liftIO $
            writeFinalSamplingStatsEntities agent entities
