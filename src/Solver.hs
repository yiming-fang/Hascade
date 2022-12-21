{-# LANGUAGE BlockArguments #-}
module Solver
      ( greedySolver
      ) where

import           BasicTypes                     ( UnweightedGraph
                                                , Vertex
                                                )
import           Control.Monad                  ( replicateM )
import           Control.Parallel.Strategies    ( parBuffer
                                                , parList
                                                , rdeepseq
                                                , rpar
                                                , rseq
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
      :: Map.Map Int [Int] -> Set Int -> Int -> Float -> Int -> Set Int
greedySolver graph vSet k thresh mcTrials
      | k == 0
      = vSet
      | otherwise
      = let runMC :: Int -> (Float, Int)
            runMC = monteCarlo graph vSet buffer

            runChunk :: [Int] -> [(Float, Int)]
            runChunk vs = map runMC vs -- `using` parList rdeepseq

            findMaxV :: [(Float, Int)] -> (Float, Int) -> Int
            findMaxV [] acc = snd acc
            findMaxV (x : xs) acc | fst x > fst acc = findMaxV xs x
                                  | otherwise       = findMaxV xs acc

            buffer      = replicate mcTrials thresh
            candidateVs = Map.keys graph
            candidateChunks = split 10 candidateVs

            -- scores      = map runChunk candidateChunks `using` parList rdeepseq
            -- vMax        = findMaxV (concat scores) (0, -1)

            scores      = map runMC candidateVs `using` parList rseq
            vMax        = findMaxV scores (0, -1)
            
            vSet'       = Set.insert vMax vSet
        in  greedySolver graph vSet' (k - 1) thresh mcTrials


split :: Int -> [a] -> [[a]]
split numChunks xs = chunk (length xs `quot` numChunks) xs


chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (as, bs) = splitAt n xs in as : chunk n bs


-- getChunks :: Set Int -> [Float] -> Int -> [(Set Int, Float)]
-- getChunks vs threshs size = 
--       let vlist = Set.toList vs


monteCarloV1
      :: Map.Map Int [Int] -> Set Int -> [Float] -> Int -> (Float, Int)
monteCarloV1 graph vSet ps vNew = (mean, vNew)
   where
      vs   = Set.insert vNew vSet
      lens = map (independentCascade graph vs 0) ps -- `using` parBuffer 10 rseq
      mean = sum lens / realToFrac (length lens)

monteCarlo
      :: Map.Map Int [Int] -> Set Int -> [Float] -> Int -> (Float, Int)
monteCarlo graph vSet ps vNew = (mean, vNew)
   where
      vs  = Set.insert vNew vSet
      pss = split 2 ps

      meansWithSizes =
            map (monteCarloChunk graph vs) pss `using` parList rdeepseq

      totalSum  = foldr ((+) . \(a, b) -> a + realToFrac b) 0 meansWithSizes
      totalSize = foldr ((+) . snd) 0 meansWithSizes
      mean      = totalSum / realToFrac totalSize



monteCarloChunk :: Map.Map Int [Int] -> Set Int -> [Float] -> (Float, Int)
monteCarloChunk graph vSet ps = (mean, length ps)
   where
      lens = map (independentCascade graph vSet 0) ps
      mean = sum lens / realToFrac (length lens)


independentCascade :: Map.Map Int [Int] -> Set Int -> Int -> Float -> Float
independentCascade graph vSet depth thresh
      | depth > 100 = 0
      | otherwise = if null newActiveSet
            then 0
            else
                  let
                        nextCascade = independentCascade graph'
                                                         activeSet
                                                         (depth + 1)
                                                         thresh
                        thisCascade = realToFrac $ length activeSet
                  in
                        thisCascade + nextCascade

   where
      graph'         = graph Map.\\ Map.fromSet (`Map.lookup` graph) newNeighborSet
      neighborSet    = getNeiborSet graph vSet
      newNeighborSet = neighborSet \\ vSet

      activatedSet   = tryActivate newNeighborSet thresh
      newActiveSet   = activatedSet \\ vSet

      activeSet      = Set.union vSet newActiveSet


randSeq :: Int -> [Float]
randSeq k = unsafePerformIO (replicateM k (randomIO :: IO Float))


tryActivate :: Set Int -> Float -> Set Int
tryActivate vs thresh = Set.fromList newActiveVs
   where
      strengths   = randSeq (length vs)
      threshs     = replicate (length vs) thresh
      diff        = zipWith (-) threshs strengths -- `using` parList rdeepseq
      threshVs    = zip diff (Set.toList vs) -- `using` parList rdeepseq
      newActive   = filter ((< 0) . fst) threshVs -- `using` parList rdeepseq
      newActiveVs = map snd newActive -- `using` parList rdeepseq


getNeiborSet :: Map.Map Int [Int] -> Set Int -> Set Int
getNeiborSet graph vSet = Set.fromList newSets
   where
      findChildren :: Int -> [Int]
      findChildren v = Data.Maybe.fromMaybe [] (Map.lookup v graph)
      newSets = concatMap findChildren $ Set.toList vSet
