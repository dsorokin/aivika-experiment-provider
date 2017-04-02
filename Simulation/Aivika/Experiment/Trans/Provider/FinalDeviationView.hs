
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Trans.Provider.FinalDeviationView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'DeviationView' that provides with
-- the deviation data in final time points.
--

module Simulation.Aivika.Experiment.Trans.Provider.FinalDeviationView 
       (FinalDeviationView(..), 
        defaultFinalDeviationView) where

import Control.Monad
import Control.Monad.Trans

import Data.Array

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Experiment
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Trans.Provider.Types

-- | Defines the 'View' that provides with the deviation data
-- in final time points.
data FinalDeviationView m =
  FinalDeviationView { finalDeviationKey :: String,
                       -- ^ The source key.
                       finalDeviationTitle :: String,
                       -- ^ The title.
                       finalDeviationTransform :: ResultTransform m,
                       -- ^ The transform applied to the results before receiving series.
                       finalDeviationSeries :: ResultTransform m
                       -- ^ It defines the series to provide with.
                     }
  
-- | The default Final Deviation view.  
defaultFinalDeviationView :: MonadDES m => FinalDeviationView m
{-# INLINABLE defaultFinalDeviationView #-}
defaultFinalDeviationView = 
  FinalDeviationView { finalDeviationKey       = error "Provide with the finalDeviationKey field value",
                       finalDeviationTitle     = "Final Deviation",
                       finalDeviationTransform = id,
                       finalDeviationSeries    = id }
  
instance ExperimentProviding ExperimentProvider m => ExperimentView (FinalDeviationView m) ExperimentProvider m where

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
                => FinalDeviationView m
                -> ExperimentContext ExperimentProvider m
                -> ExperimentData m
                -> Composite m ()
{-# INLINABLE simulateView #-}
simulateView view ctx expdata =
  do let rs      = finalDeviationSeries view $
                   finalDeviationTransform view $
                   experimentResults expdata
         exts    = resultsToDoubleStatsValues rs
         signals = experimentPredefinedSignals expdata
         signal  = resultSignalInStopTime signals
         srcKey  = finalDeviationKey view
         title   = finalDeviationTitle view
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
                                         dataSourceId = srcId,
                                         dataItem = item }
               liftIO $
                 aggregateInFinalDeviationEntity aggregator entity
