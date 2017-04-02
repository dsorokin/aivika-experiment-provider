
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Provider.LastValueView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'LastValueView' that provides with
-- the simulation results in final time points.
--

module Simulation.Aivika.Experiment.Provider.LastValueView 
       (LastValueView(..), 
        defaultLastValueView) where

import Control.Monad
import Control.Monad.Trans

import Data.Array

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Provider.Types

-- | Defines the 'View' that provides with the simulation results
-- in final time points.
data LastValueView =
  LastValueView { lastValueKey :: SourceKey,
                  -- ^ The source key.
                  lastValueTitle :: String,
                  -- ^ The title.
                  lastValueTransform :: ResultTransform,
                  -- ^ The transform applied to the results before receiving series.
                  lastValueSeries :: ResultTransform
                  -- ^ It defines the series to provide with.
                }
  
-- | The default Last Value view.  
defaultLastValueView :: LastValueView
defaultLastValueView = 
  LastValueView { lastValueKey       = error "Provide with the lastValueKey field value",
                  lastValueTitle     = "Last Value",
                  lastValueTransform = expandResults,
                  lastValueSeries    = id }
  
instance ExperimentView LastValueView ExperimentProvider where

  outputView v = 
    let reporter exp provider env =
          let ctx = makeExperimentProviderContext env
          in return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateView v ctx,
                                         reporterContext    = ctx }
    in ExperimentGenerator { generateReporter = reporter }
       
-- | Provide with the simulation results.
simulateView :: LastValueView
                -> ExperimentContext ExperimentProvider
                -> ExperimentData
                -> Composite ()
simulateView view ctx expdata =
  do let rs      = lastValueSeries view $
                   lastValueTransform view $
                   experimentResults expdata
         exts    = resultsToDoubleValues rs
         signals = experimentPredefinedSignals expdata
         signal  = resultSignalInStopTime signals
         srcKey  = lastValueKey view
         title   = lastValueTitle view
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
          srcEntity <- liftIO $ readOrCreateSourceEntity agent expId srcKey title ns
          let vars  = sourceVarEntities srcEntity
              srcId = sourceId srcEntity
          entities <- forM (zip vars exts) $ \(var, ext) ->
            do t <- liftDynamics time
               n <- liftDynamics integIteration
               a <- resultValueData ext
               entityId <- liftIO newRandomUUID
               let item   = DataItem { dataItemValue = a,
                                       dataItemIteration = n,
                                       dataItemTime = t }
                   entity = DataEntity { dataId = entityId,
                                         dataExperimentId = expId,
                                         dataRunIndex = i,
                                         dataVarId = varId var,
                                         dataSourceId = srcId,
                                         dataItem = item }
               return entity
          liftIO $
            writeLastValueEntities agent entities
               
