module BasicTypes where

import qualified Data.Map.Strict as Map

type Vertex = Int
type Weight = Float

type UnweightedGraph = Map.Map Vertex [Vertex]
type WeightedGraph = Map.Map Vertex [(Vertex, Weight)]


sampleG :: WeightedGraph
sampleG = Map.fromList [(1,[(2, 0.1)]),
                        (2,[(1, 0.2), (3, 0.3)]),
                        (3,[(2, 0.6), (4, 0.8)]),
                        (4,[(3, 0.1)])]