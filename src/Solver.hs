{-# LANGUAGE BlockArguments #-}
module Solver
      ( greedySolver
      ) where

import           BasicTypes                     ( UnweightedGraph
                                                , Vertex
                                                )
import           Control.Monad                  ( replicateM )
import           Control.Parallel.Strategies    ( parList
                                                , parMap
                                                , rpar
                                                , rseq
                                                , runEval
                                                , using
                                                , rdeepseq, parBuffer
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

            findMaxV :: [(Float, Vertex)] -> (Float, Vertex) -> Vertex
            findMaxV [] acc = snd acc
            findMaxV (x : xs) acc | fst x > fst acc = findMaxV xs x
                                  | otherwise       = findMaxV xs acc

            buffer      = replicate mcTrials thresh
            candidateVs = Map.keys graph

            scores      = map runMC candidateVs -- `using` parList rpar

            vMax        = findMaxV scores (0, -1)
            vSet'       = Set.insert vMax vSet
        in  greedySolver graph vSet' (k - 1) thresh mcTrials


split :: Int -> [a] -> [[a]]
split numChunks xs = chunk (length xs `quot` numChunks) xs


chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (as,bs) = splitAt n xs in as : chunk n bs


-- getChunks :: Set Vertex -> [Float] -> Int -> [(Set Vertex, Float)]
-- getChunks vs threshs size = 
--       let vlist = Set.toList vs


monteCarloV1
      :: UnweightedGraph -> Set Vertex -> [Float] -> Vertex -> (Float, Vertex)
monteCarloV1 graph vSet ps vNew = (mean, vNew)
   where
      vs   = Set.insert vNew vSet
      lens  = map (independentCascade graph vs 0) ps -- `using` parList rseq
      mean = sum lens / realToFrac (length lens)

monteCarlo
      :: UnweightedGraph -> Set Vertex -> [Float] -> Vertex -> (Float, Vertex)
monteCarlo graph vSet ps vNew = (mean, vNew)
   where
      vs   = Set.insert vNew vSet
      pss  = split 500 ps
      meansWithSizes = map (monteCarloChunk graph vs) pss `using` parBuffer 10 rseq
      totalSum = foldr ((+) . \(a, b) -> a + realToFrac b) 0 meansWithSizes
      totalSize = foldr ((+) . snd) 0 meansWithSizes
      mean = totalSum / realToFrac totalSize



monteCarloChunk :: UnweightedGraph -> Set Vertex -> [Float] -> (Float, Int)
monteCarloChunk graph vSet ps = (mean, length ps)
      where
            lens  = map (independentCascade graph vSet 0) ps
            mean = sum lens / realToFrac (length lens)


independentCascade :: UnweightedGraph -> Set Vertex -> Int -> Float -> Float
independentCascade graph vSet  depth thresh
      | depth > 50 = 0
      | otherwise = if null newActiveSet
            then 0
            else
                  let nextCascade = independentCascade graph activeSet  (depth + 1) thresh
                      thisCascade = realToFrac $ length activeSet
                  in  thisCascade + nextCascade

            where
                  neighborSet    = getNeiborSet graph vSet
                  newNeighborSet = neighborSet \\ vSet

                  activatedSet   = tryActivate newNeighborSet thresh
                  newActiveSet   = activatedSet \\ vSet

                  activeSet      = Set.union vSet newActiveSet


randSeq :: Int -> [Float]
randSeq k = unsafePerformIO (replicateM k (randomIO :: IO Float))


tryActivate :: Set Vertex -> Float -> Set Vertex
tryActivate vs thresh = Set.fromList newActiveVs
   where
      strengths   = randSeq (length vs)
      threshs     = replicate (length vs) thresh
      diff        = zipWith (-) threshs strengths
      threshVs    = zip diff $ Set.toList vs
      newActive   = filter ((< 0) . fst) threshVs
      newActiveVs = map snd newActive


getNeiborSet :: UnweightedGraph -> Set Vertex -> Set Vertex
getNeiborSet graph vSet = Set.fromList newSets
   where
      findChildren :: Vertex -> [Vertex]
      findChildren v = Data.Maybe.fromMaybe [] (Map.lookup v graph)
      newSets = concatMap findChildren $ Set.toList vSet
