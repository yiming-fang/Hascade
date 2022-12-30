{-# LANGUAGE BlockArguments #-}
module Solver
      ( greedySolver
      ) where

import           BasicTypes                     ( UnweightedGraph
                                                , Vertex
                                                )
import           Control.Monad                  ( replicateM )
import           Control.Parallel.Strategies    ( parList
                                                , rdeepseq
                                                , using
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set
                                                , (\\)
                                                )
import           System.IO.Unsafe               ( unsafePerformIO )
import           System.Random                  ( randomIO )



greedySolver
      :: UnweightedGraph -> Set Vertex -> Int -> Float -> Int -> Set Vertex
greedySolver graph vSet k thresh mcTrials
      | k == 0
      = vSet
      | otherwise
      = let runMC :: Vertex -> (Float, Vertex)
            runMC = monteCarlo graph vSet buffer

            runChunk :: [Vertex] -> [(Float, Vertex)]
            runChunk vs = map runMC vs

            findMaxV :: [(Float, Vertex)] -> (Float, Vertex) -> Vertex
            findMaxV [] acc = snd acc
            findMaxV (x : xs) acc | fst x > fst acc = findMaxV xs x
                                  | otherwise       = findMaxV xs acc

            buffer      = replicate mcTrials thresh
            candidateVs = Map.keys graph

            scores      = map runMC candidateVs
            vMax        = findMaxV scores (0, -1)

            vSet'       = Set.insert vMax vSet
        in  greedySolver graph vSet' (k - 1) thresh mcTrials


split :: Int -> [a] -> [[a]]
split numChunks xs = chunk (length xs `quot` numChunks) xs


chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (as, bs) = splitAt n xs in as : chunk n bs


monteCarlo
      :: UnweightedGraph -> Set Vertex -> [Float] -> Vertex -> (Float, Vertex)
monteCarlo graph vSet ps vNew = (mean, vNew)
   where
      meansWithSizes = map mc pss `using` parList rdeepseq

      vs             = Set.insert vNew vSet
      pss            = split 4 ps

      mc             = monteCarloChunk graph vs

      totalSum       = foldr ((+) . multSize) 0 meansWithSizes
      totalSize      = foldr ((+) . snd) 0 meansWithSizes

      multSize (a, b) = a * realToFrac b
      mean = totalSum / realToFrac totalSize


monteCarloChunk :: UnweightedGraph -> Set Vertex -> [Float] -> (Float, Vertex)
monteCarloChunk graph vSet ps = (mean, length ps)
   where
      lens = map (independentCascade graph vSet 0) ps
      mean = sum lens / realToFrac (length lens)


independentCascade :: UnweightedGraph -> Set Vertex -> Int -> Float -> Float
independentCascade graph vSet depth thresh = if null activatedSet'
      then 0
      else
            let nextCascade =
                      independentCascade graph' activeSet (depth + 1) thresh
                thisCascade = realToFrac $ length activeSet
            in  thisCascade + nextCascade

   where
      graph'        = graph Map.\\ setMap
      setMap        = Map.fromSet (`Map.lookup` graph) neighborSet'

      neighborSet   = getNeiborSet graph vSet
      neighborSet'  = neighborSet \\ vSet

      activatedSet  = tryActivate neighborSet' thresh
      activatedSet' = activatedSet \\ vSet

      activeSet     = Set.union vSet activatedSet'


randSeq :: Int -> [Float]
randSeq k = unsafePerformIO (replicateM k (randomIO :: IO Float))


tryActivate :: Set Vertex -> Float -> Set Vertex
tryActivate vs thresh = Set.fromList newActiveVs
   where
      strengths   = randSeq l
      threshs     = replicate l thresh
      l           = length vs

      diff        = zipWith (-) threshs strengths
      threshVs    = zip diff (Set.toList vs)
      newActive   = filter ((< 0) . fst) threshVs

      newActiveVs = map snd newActive


getNeiborSet :: UnweightedGraph -> Set Vertex -> Set Vertex
getNeiborSet graph vSet = Set.fromList newSets
   where
      findChildren :: Int -> [Int]
      findChildren v = Data.Maybe.fromMaybe [] (Map.lookup v graph)

      newSets = concatMap findChildren $ Set.toList vSet
