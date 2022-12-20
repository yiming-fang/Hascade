module Solver where

import BasicTypes
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

IndependentCascade :: Graph -> Set.Set Vertex -> Float -> Float
IndependentCascade graph seeds prob = 0.0
    where newSeeds = map (bfs graph) seeds


getNeiborSet :: Graph -> Set.Set Vertex -> Set.Set Vertex
getNeiborSet graph seeds = Set.map 
      where
        newFrontier = Set.fromList (concatMap findchild (Set.toList frontier))
        newExplored = explored ++ Set.toList frontier
        findchild :: Int -> [Int]
        findchild i
          | Map.lookup i g == Nothing = error "invalid key for lookup"
          | otherwise = filter (\neighbor-> notElem neighbor explored) adjList
            where (Just adjList) = Map.lookup i g