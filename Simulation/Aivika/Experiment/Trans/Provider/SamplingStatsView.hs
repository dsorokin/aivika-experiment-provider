
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Trans.Provider.SamplingStatsView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'SamplingStatsView' that provides with
-- sample-based statistics data in time points.
--

module Simulation.Aivika.Experiment.Trans.Provider.SamplingStatsView 
       (SamplingStatsView(..), 
        defaultSamplingStatsView) where

import Control.Monad
import Control.Monad.Trans

import Data.Array

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Experiment
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Trans.Provider.Types

-- | Defines the 'View' that provides with sample-based statistics data
-- in time points.
data SamplingStatsView m =
  SamplingStatsView { samplingStatsKey :: SourceKey,
                      -- ^ The source key.
                      samplingStatsTitle :: String,
                      -- ^ The title.
                      samplingStatsDescription :: String,
                      -- ^ The description.
                      samplingStatsPredicate :: Event m Bool,
                      -- ^ It specifies the predicate that filters data.
                      samplingStatsTransform :: ResultTransform m,
                      -- ^ The transform applied to the results before receiving the series.
                      samplingStatsSeries :: ResultTransform m
                      -- ^ It defines the series to provide with.
                    }
  
-- | The default 'SamplingStatsView'.  
defaultSamplingStatsView :: MonadDES m => SamplingStatsView m
{-# INLINABLE defaultSamplingStatsView #-}
defaultSamplingStatsView = 
  SamplingStatsView { samplingStatsKey       = error "Provide with the samplingStatsKey field value",
                      samplingStatsTitle     = "Sample-based Statistics",
                      samplingStatsDescription = "",
                      samplingStatsPredicate = return True,
                      samplingStatsTransform = id,
                      samplingStatsSeries    = id }
  
instance ExperimentProviding ExperimentProvider m => ExperimentView SamplingStatsView ExperimentProvider m where

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
                => SamplingStatsView m
                -> ExperimentContext ExperimentProvider m
                -> ExperimentData m
                -> Composite m ()
{-# INLINABLE simulateView #-}
simulateView view ctx expdata =
  do let rs      = samplingStatsSeries view $
                   samplingStatsTransform view $
                   experimentResults expdata
         exts    = resultsToDoubleStatsValues rs
         signals = experimentPredefinedSignals expdata
         signal  = pureResultSignal signals $
                   resultSignal rs
         srcKey    = samplingStatsKey view
         title     = samplingStatsTitle view
         descr     = samplingStatsDescription view
         predicate = samplingStatsPredicate view
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
          srcEntity <- liftIO $ readOrCreateSourceEntityByKey agent expId srcKey title descr ns SamplingStatsEntityType
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
                 writeSamplingStatsEntity agent entity
