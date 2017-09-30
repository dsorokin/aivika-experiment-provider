
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Trans.Provider.MultipleLastValueListView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'MultipleLastValueListView' that provides with
-- multiple simulation results in final time points accumulated for all runs.
--

module Simulation.Aivika.Experiment.Trans.Provider.MultipleLastValueListView 
       (MultipleLastValueListView(..), 
        defaultMultipleLastValueListView) where

import Control.Monad
import Control.Monad.Trans

import Data.Array

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Experiment
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Trans.Provider.Types

-- | Defines the 'View' that provides with multiple simulation results
-- in final time points accumulated for all runs.
data MultipleLastValueListView m =
  MultipleLastValueListView { multipleLastValueListKey :: SourceKey,
                              -- ^ The source key.
                              multipleLastValueListTitle :: String,
                              -- ^ The title.
                              multipleLastValueListDescription :: String,
                              -- ^ The description.
                              multipleLastValueListTransform :: ResultTransform m,
                              -- ^ The transform applied to the results before receiving series.
                              multipleLastValueListSeries :: ResultTransform m
                              -- ^ It defines the series to provide with.
                            }
  
-- | The default Multiple Value List view.  
defaultMultipleLastValueListView :: MonadDES m => MultipleLastValueListView m
{-# INLINABLE defaultMultipleLastValueListView #-}
defaultMultipleLastValueListView = 
  MultipleLastValueListView { multipleLastValueListKey         = error "Provide with the multipleLastValueListKey field value",
                              multipleLastValueListTitle       = "Multiple Last Value List",
                              multipleLastValueListDescription = "",
                              multipleLastValueListTransform   = expandResults,
                              multipleLastValueListSeries      = id }
  
instance ExperimentProviding ExperimentProvider m => ExperimentView MultipleLastValueListView ExperimentProvider m where

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
                => MultipleLastValueListView m
                -> ExperimentContext ExperimentProvider m
                -> ExperimentData m
                -> Composite m ()
{-# INLINABLE simulateView #-}
simulateView view ctx expdata =
  do let rs      = multipleLastValueListSeries view $
                   multipleLastValueListTransform view $
                   experimentResults expdata
         exts    = resultsToDoubleListValues rs
         signals = experimentPredefinedSignals expdata
         signal  = resultSignalInStopTime signals
         srcKey    = multipleLastValueListKey view
         title     = multipleLastValueListTitle view
         descr     = multipleLastValueListDescription view
         env        = contextExperimentEnvironment ctx
         provider   = environmentExperimentProvider env
         aggregator = providerExperimentAggregator provider
         agent      = experimentAggregatorAgent aggregator
         exp        = environmentExperiment env
         expId      = environmentExperimentId env
         loc        = experimentLocalisation exp
     i <- liftParameter simulationIndex
     disposableComposite $
       DisposableEvent $
       do ns <- forM exts $ \ext ->
            return (localisePathResultTitle loc $ resultValueIdPath ext,
                    localisePathResultDescription loc $ resultValueIdPath ext)
          srcEntity <- liftIO $ readOrCreateSourceEntityByKey agent expId srcKey title descr ns MultipleLastValueListEntityType
          let vars  = sourceEntityVarEntities srcEntity
              srcId = sourceEntityId srcEntity
          forM_ (zip vars exts) $ \(var, ext) ->
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
               liftIO $
                 aggregateInMultipleLastValueListEntity aggregator entity
               
