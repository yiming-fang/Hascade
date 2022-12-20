{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE BlockArguments #-}
module Solver where

import           BasicTypes                     ( UnweightedGraph
                                                , Vertex
                                                )
import           Control.Monad                  ( replicateM )
import           Control.Parallel.Strategies
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set
                                                , (\\)
                                                )
import           System.Random                  ( randomIO )


greedySolver
      :: UnweightedGraph -> Set Vertex -> Int -> Float -> Int -> IO (Set Vertex)
greedySolver graph vSet k thresh mcTrials
      | k == 0 = do
            return vSet
      | otherwise = do
            let   runMC :: Vertex -> IO (Float, Vertex)
                  runMC = monteCarlo graph vSet buffer

                  findMaxV :: [(Float, Vertex)] -> (Float, Vertex) -> Vertex
                  findMaxV [] acc = snd acc
                  findMaxV (x : xs) acc | fst x > fst acc = findMaxV xs x
                                        | otherwise       = findMaxV xs acc

                  buffer = replicate mcTrials thresh

            scores <- mapM runMC (Map.keys graph)

            let vMax =  findMaxV scores (0, -1)
                vSet' = Set.insert vMax vSet

            greedySolver graph vSet' (k - 1) thresh mcTrials


monteCarlo
      :: UnweightedGraph
      -> Set Vertex
      -> [Float]
      -> Vertex
      -> IO (Float, Vertex)
monteCarlo graph vSet ps vNew = do
      let vs = Set.insert vNew vSet
      ps' <- mapM (independentCascade graph vs) ps
      let mean = sum ps' / realToFrac (length ps')
      return (mean, vNew)


independentCascade :: UnweightedGraph -> Set Vertex -> Float -> IO Float
independentCascade graph vSet thresh = do
      neighborSet  <- getNeiborSet graph vSet
      activatedSet <- tryActivate neighborSet thresh
      let   newActiveSet = activatedSet \\ vSet
            activeSet    = Set.union vSet newActiveSet
      if null newActiveSet
            then return 0
            else do
                  nextCascade <- independentCascade graph activeSet thresh
                  let thisCascade = realToFrac $ length activeSet
                  return $ thisCascade + nextCascade


randSeq :: Int -> IO [Float]
randSeq k = replicateM k (randomIO :: IO Float)


tryActivate :: Set Vertex -> Float -> IO (Set Vertex)
tryActivate vs thresh = do
      strengths <- randSeq (length vs)
      let   threshs     = replicate (length vs) thresh
            diff        = zipWith (-) threshs strengths
            threshVs    = zip diff $ Set.toList vs
            newActive   = filter ((< 0) . fst) threshVs
            newActiveVs = map snd newActive
      return $ Set.fromList newActiveVs


getNeiborSet :: UnweightedGraph -> Set Vertex -> IO (Set Vertex)
getNeiborSet graph vSet = do
      let   findChildren :: Vertex -> [Vertex]
            findChildren v = Data.Maybe.fromMaybe [] (Map.lookup v graph)
            newSets = concatMap findChildren $ Set.toList vSet
      return $ Set.fromList newSets

