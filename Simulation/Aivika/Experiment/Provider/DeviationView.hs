
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Provider.DeviationView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'DeviationView' that provides with
-- the deviation data in time points.
--

module Simulation.Aivika.Experiment.Provider.DeviationView 
       (DeviationView(..), 
        defaultDeviationView) where

import Control.Monad
import Control.Monad.Trans

import Data.Array

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Provider.Types

-- | Defines the 'View' that provides with the deviation data
-- in time points.
data DeviationView =
  DeviationView { deviationKey :: SourceKey,
                  -- ^ The source key.
                  deviationTitle :: String,
                  -- ^ The title.
                  deviationDescription :: String,
                  -- ^ The description.
                  deviationGridSize :: Int,
                  -- ^ The size of the grid, where the series data are collected.
                  deviationTransform :: ResultTransform,
                  -- ^ The transform applied to the results before receiving series.
                  deviationSeries :: ResultTransform
                  -- ^ It defines the series to provide with.
                }
  
-- | The default Deviation view.  
defaultDeviationView :: DeviationView
defaultDeviationView = 
  DeviationView { deviationKey       = error "Provide with the deviationKey field value",
                  deviationTitle     = "Deviation",
                  deviationDescription = "",
                  deviationGridSize  = 1200,
                  deviationTransform = id,
                  deviationSeries    = id }
  
instance ExperimentView DeviationView ExperimentProvider where

  outputView v = 
    let reporter exp provider env =
          let ctx = makeExperimentProviderContext env
          in return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateView v ctx,
                                         reporterContext    = ctx }
    in ExperimentGenerator { generateReporter = reporter }
       
-- | Provide with the simulation results.
simulateView :: DeviationView
                -> ExperimentContext ExperimentProvider
                -> ExperimentData
                -> Composite ()
simulateView view ctx expdata =
  do let rs      = deviationSeries view $
                   deviationTransform view $
                   experimentResults expdata
         exts    = resultsToDoubleStatsValues rs
         signals = experimentPredefinedSignals expdata
         signal  = pureResultSignal signals $
                   resultSignal rs
         srcKey    = deviationKey view
         title     = deviationTitle view
         descr     = deviationDescription view
         env        = contextExperimentEnvironment ctx
         provider   = environmentExperimentProvider env
         aggregator = providerExperimentAggregator provider
         agent      = experimentAggregatorAgent aggregator
         exp        = environmentExperiment env
         expId      = environmentExperimentId env
         loc        = experimentLocalisation exp
         getData ext n =
           do a <- resultValueData ext
              return (n, a)
     i  <- liftParameter simulationIndex
     signal <- liftEvent $
               newSignalInTimeGrid $
               deviationGridSize view
     hs <- forM exts $ \ext ->
           newSignalHistory $
           mapSignalM (getData ext) signal
     disposableComposite $
       DisposableEvent $
       do ns <- forM exts $ \ext ->
            return (localisePathResultTitle loc $ resultValueIdPath ext,
                    localisePathResultDescription loc $ resultValueIdPath ext)
          srcEntity <- liftIO $ readOrCreateSourceEntityByKey agent expId srcKey title descr ns DeviationEntityType
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
                 aggregateInDeviationEntity aggregator entity
