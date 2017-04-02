
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Trans.Provider.View.DeviationView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'DeviationView' that provides with
-- the deviation data in time points.
--

module Simulation.Aivika.Experiment.Trans.Provider.View.DeviationView 
       (DeviationView(..), 
        defaultDeviationView) where

import Control.Monad
import Control.Monad.Trans

import Data.Array

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Experiment
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Trans.Provider.Types

-- | Defines the 'View' that provides with the deviation data
-- in time points.
data DeviationView m =
  DeviationView { deviationSourceId :: SourceUUID,
                  -- ^ The source identifier.
                  deviationPredicate :: Event m Bool,
                  -- ^ It specifies the predicate that filters data.
                  deviationTransform :: ResultTransform m,
                  -- ^ The transform applied to the results before receiving series.
                  deviationSeries :: ResultTransform m
                  -- ^ It defines the series to provide with.
                }
  
-- | The default Deviation view.  
defaultDeviationView :: MonadDES m => DeviationView m
{-# INLINABLE defaultDeviationView #-}
defaultDeviationView = 
  DeviationView { deviationSourceId  = error "Provide with the deviationSourceId field value",
                  deviationPredicate = return True,
                  deviationTransform = expandResults,
                  deviationSeries    = id }
  
instance ExperimentProviding ExperimentProvider m => ExperimentView (DeviationView m) ExperimentProvider m where

  {-# INLINABLE outputView #-}
  outputView v = 
    let reporter exp provider env =
          let ctx = makeExperimentProviderContext env (deviationSourceId v) 
          in return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateView v ctx,
                                         reporterContext    = ctx }
    in ExperimentGenerator { generateReporter = reporter }
       
-- | Provide with the simulation results.
simulateView :: ExperimentProviding ExperimentProvider m
                => DeviationView m
                -> ExperimentContext ExperimentProvider m
                -> ExperimentData m
                -> Composite m ()
{-# INLINABLE simulateView #-}
simulateView view ctx expdata =
  do let rs      = deviationSeries view $
                   deviationTransform view $
                   experimentResults expdata
         exts    = resultsToDoubleStatsValues rs
         signals = experimentPredefinedSignals expdata
         signal  = pureResultSignal signals $
                   resultSignal rs
         predicate = deviationPredicate view
         env        = contextExperimentEnvironment ctx
         provider   = environmentExperimentProvider env
         aggregator = providerExperimentAggregator provider
         agent      = experimentAggregatorAgent aggregator
         exp        = environmentExperiment env
         expId      = environmentExperimentId env
         sourceId   = contextSourceId ctx
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
          vars <- liftIO $ readOrCreateVarEntities agent expId ns
          forM_ (zip vars hs) $ \(var, h) ->
            do (ts, xs) <- readSignalHistory h
               item <- forM (zip (elems ts) (elems xs)) $ \(t, (n, a)) ->
                 return DataItem { dataItemValue = a,
                                   dataItemIteration = n,
                                   dataItemTime = t }
               entityId <- liftIO newRandomUUID
               let entity = DataEntity { dataId = entityId,
                                         dataExperimentId = expId,
                                         dataRunIndex = i,
                                         dataVarId = varId var,
                                         dataSourceId = sourceId,
                                         dataItem = item }
               liftIO $
                 aggregateInDeviationEntity aggregator entity
