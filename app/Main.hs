{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Main where

import qualified Data.Map.Strict               as Map
import           System.Environment             ( getArgs
                                                , getProgName
                                                )
import           System.Exit                    ( die )
import BasicTypes
import qualified Data.Set                      as Set

import Solver

main :: IO ()
main = do
  args <- getArgs
  case args of
    [version, filename] -> do
      contents <- readFile filename
      let inputGraph = constructGraph contents
      case version of
        "sequential" -> do
          print version
          print $ greedySolver inputGraph Set.empty 1 0.5 1000
        "parallel" -> do
          print version
          -- print $ length $ bcSolverPar inputGraph
        _ -> die "Usage: Choose correct version (sequential / parallel)"
    _ -> do
      pn <- getProgName
      die $ "Usage: " ++ pn ++ "<version> <filename>"

constructGraph :: String -> UnweightedGraph
constructGraph = Map.fromList . map extractLine . lines

extractLine :: String -> (Int, [Int])
extractLine str = (node, neighbors)
  where (node, neighbors) = case words str of 
                (this : others) -> (read this, map read others)
                []              -> (-1, [])
