
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Provider.ValueListView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'ValueListView' that provides with
-- multiple simulation results in time points.
--

module Simulation.Aivika.Experiment.Provider.ValueListView 
       (ValueListView(..), 
        defaultValueListView) where

import Control.Monad
import Control.Monad.Trans

import Data.Array

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Provider.Types

-- | Defines the 'View' that provides multiple simulation results in time points.
data ValueListView =
  ValueListView { valueListKey :: SourceKey,
                  -- ^ The source key.
                  valueListTitle :: String,
                  -- ^ The title.
                  valueListDescription :: String,
                  -- ^ The description.
                  valueListPredicate :: Event Bool,
                  -- ^ It specifies the predicate that filters data.
                  valueListTransform :: ResultTransform,
                  -- ^ The transform applied to the results before receiving series.
                  valueListSeries :: ResultTransform,
                  -- ^ It defines the series to provide with.
                  valueListGridSize :: Maybe Int
                  -- ^ The size of the grid, where the series data are saved.
                }
  
-- | The default Value List view.  
defaultValueListView :: ValueListView
defaultValueListView = 
  ValueListView { valueListKey       = error "Provide with the valueListKey field value",
                  valueListTitle     = "Value List",
                  valueListDescription = "",
                  valueListPredicate = return True,
                  valueListTransform = expandResults,
                  valueListSeries    = id,
                  valueListGridSize = Nothing }
  
instance ExperimentView ValueListView ExperimentProvider where

  outputView v = 
    let reporter exp provider env =
          let ctx = makeExperimentProviderContext env
          in return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateView v ctx,
                                         reporterContext    = ctx }
    in ExperimentGenerator { generateReporter = reporter }
       
-- | Provide with the simulation results.
simulateView :: ValueListView
                -> ExperimentContext ExperimentProvider
                -> ExperimentData
                -> Composite ()
simulateView view ctx expdata =
  do let rs      = valueListSeries view $
                   valueListTransform view $
                   experimentResults expdata
         exts    = resultsToDoubleListValues rs
         signals = experimentPredefinedSignals expdata
         srcKey    = valueListKey view
         title     = valueListTitle view
         descr     = valueListDescription view
         predicate = valueListPredicate view
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
           do signal <-
                case valueListGridSize view of
                  Just m ->
                    liftEvent $
                    fmap (mapSignal $ const ()) $
                    newSignalInTimeGrid m
                  Nothing ->
                    return $
                    pureResultSignal signals $
                    resultValueSignal ext
              newSignalHistory $
                mapSignalM (const $ getData ext) $
                filterSignalM (const predicate) signal
     disposableComposite $
       DisposableEvent $
       do ns <- forM exts $ \ext ->
            return (localisePathResultTitle loc $ resultValueIdPath ext,
                    localisePathResultDescription loc $ resultValueIdPath ext)
          srcEntity <- liftIO $ readOrCreateSourceEntityByKey agent expId srcKey title descr ns ValueListEntityType
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
                 writeValueListEntity agent entity
               
