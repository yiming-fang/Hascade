{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE BlockArguments #-}
module Solver where

import BasicTypes ( Vertex, UnweightedGraph )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe ( fromJust )
import System.Random ( randomIO )
import Control.Monad (replicateM)



randSeq :: Int -> IO [Float]
randSeq k = replicateM k (randomIO :: IO Float)


independentCascade :: UnweightedGraph -> Set.Set Vertex -> Float -> IO Int
independentCascade graph seeds threshold = do
      let neighborSet = getNeiborSet graph seeds
      newActiveSet <- tryActivate neighborSet threshold
      let activeSet = Set.union seeds newActiveSet
      if null newActiveSet
            then return 0
      else do
            nextCascade <- independentCascade graph newActiveSet threshold
            return $ length activeSet + nextCascade


tryActivate :: Set.Set Vertex -> Float -> IO (Set.Set Vertex)
tryActivate vs threshold = do
      strengths <- randSeq (length vs)
      let thresholds = replicate (length vs) threshold
          diff = zipWith (-) thresholds strengths
          threshVs = zip diff $ Set.toList vs
          newActive = filter ((< 0) . fst) threshVs
          newActiveVs = map snd newActive
      return $ Set.fromList newActiveVs


getNeiborSet :: UnweightedGraph -> Set.Set Vertex -> Set.Set Vertex
getNeiborSet graph seeds = Set.fromList newSets
      where
        findChildren :: Vertex -> [Vertex]
        findChildren v = fromJust $ Map.lookup v graph
        newSets = concatMap findChildren (Set.toList seeds)
