{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module BasicTypes
    ( Vertex
    , Weight
    , UnweightedGraph
    , WeightedGraph
    ) where


import qualified Data.Map.Strict               as Map

type Vertex = Int
type Weight = Float

type UnweightedGraph = Map.Map Vertex [Vertex]
type WeightedGraph = Map.Map Vertex [(Vertex, Weight)]
