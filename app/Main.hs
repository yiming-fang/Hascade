{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Main where

import           BasicTypes                     ( UnweightedGraph )
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Solver                         ( greedySolver )
import           System.Environment             ( getArgs
                                                , getProgName
                                                )
import           System.Exit                    ( die )


main :: IO ()
main = do
  args <- getArgs
  case args of
    [num_cores, filename] -> do
      contents <- readFile filename
      let inputGraph = constructGraph contents
      print num_cores
      print $ greedySolver inputGraph Set.empty 1 0.5 100
    _ -> do
      pn <- getProgName
      die $ "Usage: " ++ pn ++ "<num_cores> <filename>"


constructGraph :: String -> UnweightedGraph
constructGraph = Map.fromList . map extractLine . lines


extractLine :: String -> (Int, [Int])
extractLine str = (node, neighbors)
 where
  (node, neighbors) = case words str of
    (this : others) -> (read this, map read others)
    []              -> (-1, [])
