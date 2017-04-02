
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Trans.Provider.LastValueListView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'LastValueListView' that provides with
-- the multiple simulation results in final time points accumulated for all runs.
--

module Simulation.Aivika.Experiment.Trans.Provider.LastValueListView 
       (LastValueListView(..), 
        defaultLastValueListView) where

import Control.Monad
import Control.Monad.Trans

import Data.Array

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Experiment
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Trans.Provider.Types

-- | Defines the 'View' that provides with the multiple simulation results
-- in final time points accumulated for all runs.
data LastValueListView m =
  LastValueListView { lastValueListKey :: SourceKey,
                      -- ^ The source key.
                      lastValueListTitle :: String,
                      -- ^ The title.
                      lastValueListDescription :: String,
                      -- ^ The description.
                      lastValueListTransform :: ResultTransform m,
                      -- ^ The transform applied to the results before receiving series.
                      lastValueListSeries :: ResultTransform m
                      -- ^ It defines the series to provide with.
                    }
  
-- | The default Value List view.  
defaultLastValueListView :: MonadDES m => LastValueListView m
{-# INLINABLE defaultLastValueListView #-}
defaultLastValueListView = 
  LastValueListView { lastValueListKey       = error "Provide with the lastValueListKey field value",
                      lastValueListTitle     = "Last Value List",
                      lastValueListDescription = "",
                      lastValueListTransform = expandResults,
                      lastValueListSeries    = id }
  
instance ExperimentProviding ExperimentProvider m => ExperimentView (LastValueListView m) ExperimentProvider m where

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
                => LastValueListView m
                -> ExperimentContext ExperimentProvider m
                -> ExperimentData m
                -> Composite m ()
{-# INLINABLE simulateView #-}
simulateView view ctx expdata =
  do let rs      = lastValueListSeries view $
                   lastValueListTransform view $
                   experimentResults expdata
         exts    = resultsToDoubleListValues rs
         signals = experimentPredefinedSignals expdata
         signal  = resultSignalInStopTime signals
         srcKey    = lastValueListKey view
         title     = lastValueListTitle view
         descr     = lastValueListDescription view
         env        = contextExperimentEnvironment ctx
         provider   = environmentExperimentProvider env
         aggregator = providerExperimentAggregator provider
         agent      = experimentAggregatorAgent aggregator
         exp        = environmentExperiment env
         expId      = environmentExperimentId env
         loc        = experimentLocalisation exp
     disposableComposite $
       DisposableEvent $
       do ns <- forM exts $ \ext ->
            return (resultValueName ext, loc $ resultValueId ext)
          srcEntity <- liftIO $ readOrCreateSourceEntity agent expId srcKey title descr ns
          let vars  = sourceVarEntities srcEntity
              srcId = sourceId srcEntity
          entities <- forM (zip vars exts) $ \(var, ext) ->
            do t <- liftDynamics time
               n <- liftDynamics integIteration
               a <- resultValueData ext
               entityId <- liftIO newRandomUUID
               let item   = DataItem { dataItemValue = a,
                                       dataItemIteration = n,
                                       dataItemTime = t }
                   entity = MultipleDataEntity { multipleDataId = entityId,
                                                 multipleDataExperimentId = expId,
                                                 multipleDataVarId = varId var,
                                                 multipleDataSourceId = srcId,
                                                 multipleDataItem = item }
               return entity
          liftIO $
            writeLastValueListEntities agent entities
               
