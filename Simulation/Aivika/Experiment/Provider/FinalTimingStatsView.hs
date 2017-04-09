
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Provider.FinalTimingStatsView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'FinalTimingStatsView' that provides with
-- time-dependent statistics data in final time points.
--

module Simulation.Aivika.Experiment.Provider.FinalTimingStatsView 
       (FinalTimingStatsView(..), 
        defaultFinalTimingStatsView) where

import Control.Monad
import Control.Monad.Trans

import Data.Array

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Provider.Types

-- | Defines the 'View' that provides with time-dependent statistics data
-- in final time points.
data FinalTimingStatsView =
  FinalTimingStatsView { finalTimingStatsKey :: String,
                           -- ^ The source key.
                           finalTimingStatsTitle :: String,
                           -- ^ The title.
                           finalTimingStatsDescription :: String,
                           -- ^ The description.
                           finalTimingStatsTransform :: ResultTransform,
                           -- ^ The transform applied to the results before receiving series.
                           finalTimingStatsSeries :: ResultTransform
                           -- ^ It defines the series to provide with.
                         }
  
-- | The default 'FinalTimingStatsView'.  
defaultFinalTimingStatsView :: FinalTimingStatsView
defaultFinalTimingStatsView = 
  FinalTimingStatsView { finalTimingStatsKey       = error "Provide with the finalTimingStatsKey field value",
                         finalTimingStatsTitle     = "Final Time-dependent Statistics",
                         finalTimingStatsDescription = "",
                         finalTimingStatsTransform = id,
                         finalTimingStatsSeries    = id }
  
instance ExperimentView FinalTimingStatsView ExperimentProvider where

  outputView v = 
    let reporter exp provider env =
          let ctx = makeExperimentProviderContext env
          in return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateView v ctx,
                                         reporterContext    = ctx }
    in ExperimentGenerator { generateReporter = reporter }
       
-- | Provide with the simulation results.
simulateView :: FinalTimingStatsView
                -> ExperimentContext ExperimentProvider
                -> ExperimentData
                -> Composite ()
simulateView view ctx expdata =
  do let rs      = finalTimingStatsSeries view $
                   finalTimingStatsTransform view $
                   experimentResults expdata
         exts    = resultsToDoubleTimingStatsValues rs
         signals = experimentPredefinedSignals expdata
         signal  = resultSignalInStopTime signals
         srcKey  = finalTimingStatsKey view
         title   = finalTimingStatsTitle view
         descr   = finalTimingStatsDescription view
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
          srcEntity <- liftIO $ readOrCreateSourceEntityByKey agent expId srcKey title descr ns FinalTimingStatsEntityType
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
            writeFinalTimingStatsEntities agent entities
