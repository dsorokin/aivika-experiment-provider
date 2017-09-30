
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Provider.FinalSamplingStatsView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'FinalSamplingStatsView' that provides with
-- sample-based statistics data in final time points.
--

module Simulation.Aivika.Experiment.Provider.FinalSamplingStatsView 
       (FinalSamplingStatsView(..), 
        defaultFinalSamplingStatsView) where

import Control.Monad
import Control.Monad.Trans

import Data.Array

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Provider.Types

-- | Defines the 'View' that provides with the sample-based statistics data
-- in final time points.
data FinalSamplingStatsView =
  FinalSamplingStatsView { finalSamplingStatsKey :: String,
                           -- ^ The source key.
                           finalSamplingStatsTitle :: String,
                           -- ^ The title.
                           finalSamplingStatsDescription :: String,
                           -- ^ The description.
                           finalSamplingStatsTransform :: ResultTransform,
                           -- ^ The transform applied to the results before receiving series.
                           finalSamplingStatsSeries :: ResultTransform
                           -- ^ It defines the series to provide with.
                         }
  
-- | The default 'FinalSamplingStatsView'.  
defaultFinalSamplingStatsView :: FinalSamplingStatsView
defaultFinalSamplingStatsView = 
  FinalSamplingStatsView { finalSamplingStatsKey       = error "Provide with the finalSamplingStatsKey field value",
                           finalSamplingStatsTitle     = "Final Sample-based Statistics",
                           finalSamplingStatsDescription = "",
                           finalSamplingStatsTransform = id,
                           finalSamplingStatsSeries    = id }
  
instance ExperimentView FinalSamplingStatsView ExperimentProvider where

  outputView v = 
    let reporter exp provider env =
          let ctx = makeExperimentProviderContext env
          in return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateView v ctx,
                                         reporterContext    = ctx }
    in ExperimentGenerator { generateReporter = reporter }
       
-- | Provide with the simulation results.
simulateView :: FinalSamplingStatsView
                -> ExperimentContext ExperimentProvider
                -> ExperimentData
                -> Composite ()
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
            return (localisePathResultTitle loc $ resultValueIdPath ext,
                    localisePathResultDescription loc $ resultValueIdPath ext)
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
