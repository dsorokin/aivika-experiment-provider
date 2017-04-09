
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Provider.TimeSeriesView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'TimeSeriesView' that provides with
-- the simulation results in time points.
--

module Simulation.Aivika.Experiment.Provider.TimeSeriesView 
       (TimeSeriesView(..), 
        defaultTimeSeriesView) where

import Control.Monad
import Control.Monad.Trans

import Data.Array

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Provider.Types

-- | Defines the 'View' that provides with the simulation results
-- in time points.
data TimeSeriesView =
  TimeSeriesView { timeSeriesKey :: SourceKey,
                   -- ^ The source key.
                   timeSeriesTitle :: String,
                   -- ^ The title.
                   timeSeriesDescription :: String,
                   -- ^ The description.
                   timeSeriesPredicate :: Event Bool,
                   -- ^ It specifies the predicate that filters data.
                   timeSeriesTransform :: ResultTransform,
                   -- ^ The transform applied to the results before receiving series.
                   timeSeries :: ResultTransform
                   -- ^ It defines the series to provide with.
                 }
  
-- | The default Time Series view.  
defaultTimeSeriesView :: TimeSeriesView
defaultTimeSeriesView = 
  TimeSeriesView { timeSeriesKey       = error "Provide with the timeSeriesKey field value",
                   timeSeriesTitle     = "Time Series",
                   timeSeriesDescription = "",
                   timeSeriesPredicate = return True,
                   timeSeriesTransform = expandResults,
                   timeSeries          = id }
  
instance ExperimentView TimeSeriesView ExperimentProvider where
  
  outputView v = 
    let reporter exp provider env =
          let ctx = makeExperimentProviderContext env
          in return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateView v ctx,
                                         reporterContext    = ctx }
    in ExperimentGenerator { generateReporter = reporter }
       
-- | Provide with the simulation results.
simulateView :: TimeSeriesView
                -> ExperimentContext ExperimentProvider
                -> ExperimentData
                -> Composite ()
simulateView view ctx expdata =
  do let rs      = timeSeries view $
                   timeSeriesTransform view $
                   experimentResults expdata
         exts    = resultsToDoubleValues rs
         signals = experimentPredefinedSignals expdata
         signal  = pureResultSignal signals $
                   resultSignal rs
         srcKey    = timeSeriesKey view
         title     = timeSeriesTitle view
         descr     = timeSeriesDescription view
         predicate = timeSeriesPredicate view
         env        = contextExperimentEnvironment ctx
         provider   = environmentExperimentProvider env
         aggregator = providerExperimentAggregator provider
         agent      = experimentAggregatorAgent aggregator
         exp        = environmentExperiment env
         expId      = environmentExperimentId env
         loc        = experimentLocalisation exp
         getData ext =
           do n <- liftDynamics integIteration
              a <- resultValueData ext
              return (n, a)
     i  <- liftParameter simulationIndex
     hs <- forM exts $ \ext ->
           newSignalHistory $
           mapSignalM (const $ getData ext) $
           filterSignalM (const predicate) $
           pureResultSignal signals $
           resultValueSignal ext
     disposableComposite $
       DisposableEvent $
       do ns <- forM exts $ \ext ->
            return (resultValueName ext, loc $ resultValueId ext)
          srcEntity <- liftIO $ readOrCreateSourceEntityByKey agent expId srcKey title descr ns TimeSeriesEntityType
          let vars  = sourceEntityVarEntities srcEntity
              srcId = sourceEntityId srcEntity
          forM_ (zip vars hs) $ \(var, h) ->
            do (ts, xs) <- readSignalHistory h
               item <- forM (zip (elems ts) (elems xs)) $ \(t, (n, a)) ->
                 return DataItem { dataItemValue = a,
                                   dataItemIteration = n,
                                   dataItemTime = t }
               entityId <- liftIO newRandomUUID
               let entity = DataEntity { dataEntityId = entityId,
                                         dataEntityExperimentId = expId,
                                         dataEntityRunIndex = i,
                                         dataEntityVarId = varEntityId var,
                                         dataEntitySourceId = srcId,
                                         dataEntityItem = item }
               liftIO $
                 writeTimeSeriesEntity agent entity
               
