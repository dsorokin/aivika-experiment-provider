
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Trans.Provider.LastValueView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'LastValueView' that provides with
-- the simulation results in final time points.
--

module Simulation.Aivika.Experiment.Trans.Provider.LastValueView 
       (LastValueView(..), 
        defaultLastValueView) where

import Control.Monad
import Control.Monad.Trans

import Data.Array

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Experiment
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Trans.Provider.Types

-- | Defines the 'View' that provides with the simulation results
-- in final time points.
data LastValueView m =
  LastValueView { lastValueKey :: SourceKey,
                  -- ^ The source key.
                  lastValueTitle :: String,
                  -- ^ The title.
                  lastValueDescription :: String,
                  -- ^ The description.
                  lastValueTransform :: ResultTransform m,
                  -- ^ The transform applied to the results before receiving series.
                  lastValueSeries :: ResultTransform m
                  -- ^ It defines the series to provide with.
                }
  
-- | The default Last Value view.  
defaultLastValueView :: MonadDES m => LastValueView m
{-# INLINABLE defaultLastValueView #-}
defaultLastValueView = 
  LastValueView { lastValueKey       = error "Provide with the lastValueKey field value",
                  lastValueTitle     = "Last Value",
                  lastValueDescription = "",
                  lastValueTransform = expandResults,
                  lastValueSeries    = id }
  
instance ExperimentProviding ExperimentProvider m => ExperimentView (LastValueView m) ExperimentProvider m where

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
                => LastValueView m
                -> ExperimentContext ExperimentProvider m
                -> ExperimentData m
                -> Composite m ()
{-# INLINABLE simulateView #-}
simulateView view ctx expdata =
  do let rs      = lastValueSeries view $
                   lastValueTransform view $
                   experimentResults expdata
         exts    = resultsToDoubleValues rs
         signals = experimentPredefinedSignals expdata
         signal  = resultSignalInStopTime signals
         srcKey  = lastValueKey view
         title   = lastValueTitle view
         descr   = lastValueDescription view
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
          srcEntity <- liftIO $ readOrCreateSourceEntityByKey agent expId srcKey title descr ns LastValueEntityType
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
            writeLastValueEntities agent entities
               
