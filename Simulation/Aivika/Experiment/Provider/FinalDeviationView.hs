
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Provider.FinalDeviationView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'DeviationView' that provides with
-- the deviation data in final time points.
--

module Simulation.Aivika.Experiment.Provider.FinalDeviationView 
       (FinalDeviationView(..), 
        defaultFinalDeviationView) where

import Control.Monad
import Control.Monad.Trans

import Data.Array

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Provider.Types

-- | Defines the 'View' that provides with the deviation data
-- in final time points.
data FinalDeviationView =
  FinalDeviationView { finalDeviationSourceId :: SourceUUID,
                       -- ^ The source identifier.
                       finalDeviationTransform :: ResultTransform,
                       -- ^ The transform applied to the results before receiving series.
                       finalDeviationSeries :: ResultTransform
                       -- ^ It defines the series to provide with.
                     }
  
-- | The default Final Deviation view.  
defaultFinalDeviationView :: FinalDeviationView
defaultFinalDeviationView = 
  FinalDeviationView { finalDeviationSourceId  = error "Provide with the finalDeviationSourceId field value",
                       finalDeviationTransform = id,
                       finalDeviationSeries    = id }
  
instance ExperimentView FinalDeviationView ExperimentProvider where

  outputView v = 
    let reporter exp provider env =
          let ctx = makeExperimentProviderContext env (finalDeviationSourceId v) 
          in return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateView v ctx,
                                         reporterContext    = ctx }
    in ExperimentGenerator { generateReporter = reporter }
       
-- | Provide with the simulation results.
simulateView :: FinalDeviationView
                -> ExperimentContext ExperimentProvider
                -> ExperimentData
                -> Composite ()
simulateView view ctx expdata =
  do let rs      = finalDeviationSeries view $
                   finalDeviationTransform view $
                   experimentResults expdata
         exts    = resultsToDoubleStatsValues rs
         signals = experimentPredefinedSignals expdata
         signal  = resultSignalInStopTime signals
         env        = contextExperimentEnvironment ctx
         provider   = environmentExperimentProvider env
         aggregator = providerExperimentAggregator provider
         agent      = experimentAggregatorAgent aggregator
         exp        = environmentExperiment env
         expId      = environmentExperimentId env
         sourceId   = contextSourceId ctx
         loc        = experimentLocalisation exp
     i  <- liftParameter simulationIndex
     disposableComposite $
       DisposableEvent $
       do ns <- forM exts $ \ext ->
            return (resultValueName ext, loc $ resultValueId ext)
          vars <- liftIO $ readOrCreateVarEntities agent expId ns
          forM_ (zip vars exts) $ \(var, ext) ->
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
                                         dataSourceId = sourceId,
                                         dataItem = item }
               liftIO $
                 aggregateInFinalDeviationEntity aggregator entity
