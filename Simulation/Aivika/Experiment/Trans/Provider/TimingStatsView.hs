
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Trans.Provider.TimingStatsView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'TimingStatsView' that provides with
-- time-dependent statistics data in time points.
--

module Simulation.Aivika.Experiment.Trans.Provider.TimingStatsView 
       (TimingStatsView(..), 
        defaultTimingStatsView) where

import Control.Monad
import Control.Monad.Trans

import Data.Array

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Experiment
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Trans.Provider.Types

-- | Defines the 'View' that provides with time-dependent statistics data
-- in time points.
data TimingStatsView m =
  TimingStatsView { timingStatsKey :: SourceKey,
                    -- ^ The source key.
                    timingStatsTitle :: String,
                    -- ^ The title.
                    timingStatsDescription :: String,
                    -- ^ The description.
                    timingStatsPredicate :: Event m Bool,
                    -- ^ It specifies the predicate that filters data.
                    timingStatsTransform :: ResultTransform m,
                    -- ^ The transform applied to the results before receiving the series.
                    timingStatsSeries :: ResultTransform m
                    -- ^ It defines the series to provide with.
                  }
  
-- | The default 'TimingStatsView'.  
defaultTimingStatsView :: MonadDES m => TimingStatsView m
{-# INLINABLE defaultTimingStatsView #-}
defaultTimingStatsView = 
  TimingStatsView { timingStatsKey       = error "Provide with the timingStatsKey field value",
                    timingStatsTitle     = "Time-dependent Statistics",
                    timingStatsDescription = "",
                    timingStatsPredicate = return True,
                    timingStatsTransform = id,
                    timingStatsSeries    = id }
  
instance ExperimentProviding ExperimentProvider m => ExperimentView (TimingStatsView m) ExperimentProvider m where

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
                => TimingStatsView m
                -> ExperimentContext ExperimentProvider m
                -> ExperimentData m
                -> Composite m ()
{-# INLINABLE simulateView #-}
simulateView view ctx expdata =
  do let rs      = timingStatsSeries view $
                   timingStatsTransform view $
                   experimentResults expdata
         exts    = resultsToDoubleTimingStatsValues rs
         signals = experimentPredefinedSignals expdata
         signal  = pureResultSignal signals $
                   resultSignal rs
         srcKey    = timingStatsKey view
         title     = timingStatsTitle view
         descr     = timingStatsDescription view
         predicate = timingStatsPredicate view
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
          srcEntity <- liftIO $ readOrCreateSourceEntityByKey agent expId srcKey title descr ns TimingStatsEntityType
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
                 writeTimingStatsEntity agent entity
