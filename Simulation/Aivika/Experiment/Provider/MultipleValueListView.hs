
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Provider.MultipleValueListView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'MultipleValueListView' that provides with
-- the multiple simulation results in time points accumulated for all runs.
--

module Simulation.Aivika.Experiment.Provider.MultipleValueListView 
       (MultipleValueListView(..), 
        defaultMultipleValueListView) where

import Control.Monad
import Control.Monad.Trans

import Data.Array

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Provider.Types

-- | Defines the 'View' that provides with multiple simulation results
-- in time points accumulated for all runs.
data MultipleValueListView =
  MultipleValueListView { multipleValueListKey :: SourceKey,
                          -- ^ The source key.
                          multipleValueListTitle :: String,
                          -- ^ The title.
                          multipleValueListDescription :: String,
                          -- ^ The description.
                          multipleValueListPredicate :: Event Bool,
                          -- ^ It specifies the predicate that filters data.
                          multipleValueListTransform :: ResultTransform,
                          -- ^ The transform applied to the results before receiving series.
                          multipleValueListSeries :: ResultTransform
                          -- ^ It defines the series to provide with.
                        }
  
-- | The default Multiple Value List view.  
defaultMultipleValueListView :: MultipleValueListView
defaultMultipleValueListView = 
  MultipleValueListView { multipleValueListKey       = error "Provide with the multipleValueListKey field value",
                          multipleValueListTitle     = "Accumulated Value List",
                          multipleValueListDescription = "",
                          multipleValueListPredicate = return True,
                          multipleValueListTransform = expandResults,
                          multipleValueListSeries    = id }
  
instance ExperimentView MultipleValueListView ExperimentProvider where

  outputView v = 
    let reporter exp provider env =
          let ctx = makeExperimentProviderContext env
          in return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateView v ctx,
                                         reporterContext    = ctx }
    in ExperimentGenerator { generateReporter = reporter }
       
-- | Provide with the simulation results.
simulateView :: MultipleValueListView
                -> ExperimentContext ExperimentProvider
                -> ExperimentData
                -> Composite ()
simulateView view ctx expdata =
  do let rs      = multipleValueListSeries view $
                   multipleValueListTransform view $
                   experimentResults expdata
         exts    = resultsToDoubleListValues rs
         signals = experimentPredefinedSignals expdata
         signal  = pureResultSignal signals $
                   resultSignal rs
         srcKey    = multipleValueListKey view
         title     = multipleValueListTitle view
         descr     = multipleValueListDescription view
         predicate = multipleValueListPredicate view
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
     i <- liftParameter simulationIndex
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
          srcEntity <- liftIO $ readOrCreateSourceEntityByKey agent expId srcKey title descr ns MultipleValueListEntityType
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
                 aggregateInMultipleValueListEntity aggregator entity
               
