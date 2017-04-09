
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Provider.LastValueListView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'LastValueListView' that provides with
-- multiple simulation results in final time points.
--

module Simulation.Aivika.Experiment.Provider.LastValueListView 
       (LastValueListView(..), 
        defaultLastValueListView) where

import Control.Monad
import Control.Monad.Trans

import Data.Array

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Provider.Types

-- | Defines the 'View' that provides with multiple simulation results
-- in final time points.
data LastValueListView =
  LastValueListView { lastValueListKey :: SourceKey,
                      -- ^ The source key.
                      lastValueListTitle :: String,
                      -- ^ The title.
                      lastValueListDescription :: String,
                      -- ^ The description.
                      lastValueListTransform :: ResultTransform,
                      -- ^ The transform applied to the results before receiving series.
                      lastValueListSeries :: ResultTransform
                      -- ^ It defines the series to provide with.
                    }
  
-- | The default Value List view.  
defaultLastValueListView :: LastValueListView
defaultLastValueListView = 
  LastValueListView { lastValueListKey       = error "Provide with the lastValueListKey field value",
                      lastValueListTitle     = "Last Value List",
                      lastValueListDescription = "",
                      lastValueListTransform = expandResults,
                      lastValueListSeries    = id }
  
instance ExperimentView LastValueListView ExperimentProvider where

  outputView v = 
    let reporter exp provider env =
          let ctx = makeExperimentProviderContext env
          in return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateView v ctx,
                                         reporterContext    = ctx }
    in ExperimentGenerator { generateReporter = reporter }
       
-- | Provide with the simulation results.
simulateView :: LastValueListView
                -> ExperimentContext ExperimentProvider
                -> ExperimentData
                -> Composite ()
simulateView view ctx expdata =
  do let rs      = lastValueListSeries view $
                   lastValueListTransform view $
                   experimentResults expdata
         exts    = resultsToDoubleListValues rs
         signals = experimentPredefinedSignals expdata
         signal  = resultSignalInStopTime signals
         srcKey    = lastValueListKey view
         title     = lastValueListTitle view
         descr     = lastValueListDescription view
         env        = contextExperimentEnvironment ctx
         provider   = environmentExperimentProvider env
         aggregator = providerExperimentAggregator provider
         agent      = experimentAggregatorAgent aggregator
         exp        = environmentExperiment env
         expId      = environmentExperimentId env
         loc        = experimentLocalisation exp
     i <- liftParameter simulationIndex
     disposableComposite $
       DisposableEvent $
       do ns <- forM exts $ \ext ->
            return (resultValueName ext, loc $ resultValueId ext)
          srcEntity <- liftIO $ readOrCreateSourceEntityByKey agent expId srcKey title descr ns LastValueListEntityType
          let vars  = sourceEntityVarEntities srcEntity
              srcId = sourceEntityId srcEntity
          entities <- forM (zip vars exts) $ \(var, ext) ->
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
            writeLastValueListEntities agent entities
               
