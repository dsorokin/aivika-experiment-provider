
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Provider.SamplingStatsView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'SamplingStatsView' that provides with
-- sample-based statistics data in time points.
--

module Simulation.Aivika.Experiment.Provider.SamplingStatsView 
       (SamplingStatsView(..), 
        defaultSamplingStatsView) where

import Control.Monad
import Control.Monad.Trans

import Data.Array

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Provider.Types

-- | Defines the 'View' that provides with sample-based statistics data
-- in time points.
data SamplingStatsView =
  SamplingStatsView { samplingStatsKey :: SourceKey,
                      -- ^ The source key.
                      samplingStatsTitle :: String,
                      -- ^ The title.
                      samplingStatsDescription :: String,
                      -- ^ The description.
                      samplingStatsPredicate :: Event Bool,
                      -- ^ It specifies the predicate that filters data.
                      samplingStatsTransform :: ResultTransform,
                      -- ^ The transform applied to the results before receiving the series.
                      samplingStatsSeries :: ResultTransform,
                      -- ^ It defines the series to provide with.
                      samplingStatsGridSize :: Maybe Int
                      -- ^ The size of the grid, where the series data are saved.
                    }
  
-- | The default 'SamplingStatsView'.  
defaultSamplingStatsView :: SamplingStatsView
defaultSamplingStatsView = 
  SamplingStatsView { samplingStatsKey       = error "Provide with the samplingStatsKey field value",
                      samplingStatsTitle     = "Sample-based Statistics",
                      samplingStatsDescription = "",
                      samplingStatsPredicate = return True,
                      samplingStatsTransform = id,
                      samplingStatsSeries    = id,
                      samplingStatsGridSize  = Just 1200 }
  
instance ExperimentView SamplingStatsView ExperimentProvider where

  outputView v = 
    let reporter exp provider env =
          let ctx = makeExperimentProviderContext env
          in return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateView v ctx,
                                         reporterContext    = ctx }
    in ExperimentGenerator { generateReporter = reporter }
       
-- | Provide with the simulation results.
simulateView :: SamplingStatsView
                -> ExperimentContext ExperimentProvider
                -> ExperimentData
                -> Composite ()
simulateView view ctx expdata =
  do let rs      = samplingStatsSeries view $
                   samplingStatsTransform view $
                   experimentResults expdata
         exts    = resultsToDoubleStatsValues rs
         signals = experimentPredefinedSignals expdata
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
           do signal <-
                case samplingStatsGridSize view of
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
