module Animator ( start_animation ) where

import qualified Data.HashMap.Lazy as M
import GHC.Float (double2Float, float2Int, int2Float)
import Graphics.Gloss
import Constructor

type Node       = (Int, (Float, Float))
type Edge       = (Int, (Int, Int))
type XYoffset   = (Int, (Float, Float))

{-
list_nodes :: [Node]
list_nodes = [(1, (0, 0)),
              (2, (1, 0)),
              (3, (0, 1)),
              (4, (1, 1))]

list_edges :: [Edge]
list_edges = [(1, (1, 3)),
              (2, (2, 4)),
              (3, (3, 4)),
              (4, (1, 4)),
              (5, (2, 3))]


nodes = M.fromList list_nodes
edges = M.fromList list_edges

-}

to_animate_float :: [Float]
to_animate_float =  [double2Float value | value <- Constructor.to_animate]

nodes = M.fromList [(id, (double2Float x, double2Float y)) | (id, (x, y)) <- M.toList Constructor.nodes]


xyOffsets = M.fromList (makeOffsets to_animate_float 1)

makeOffsets :: [Float] -> Int -> [(Int, (Float, Float))]
makeOffsets [] _        = []
makeOffsets (x1:x2:xs) i    = (i,(x1, x2)) : makeOffsets xs (i+1)

-- Display parameters

width, height, leftOffset, topOffset :: Int
width       = 500
height      = 500
leftOffset  = (float2Int maxXY `div` 2) + 25
topOffset   = (float2Int maxXY `div` 2) + 25
speed       = 3 :: Float
amplitude   = 2 :: Float
scale       = maxXY / (maxXY / int2Float (width + 50)) :: Float

minX        = minimum (map fst (map snd (M.toList Animator.nodes)))
minY        = minimum (map snd (map snd (M.toList Animator.nodes)))
maxX        = maximum (map fst (map snd (M.toList Animator.nodes)))
maxY        = maximum (map snd (map snd (M.toList Animator.nodes)))
maxXY       = maximum [(maxX - minX), (maxY - minY)]

window :: Display
window = InWindow "Vibration Display" (width, height) (leftOffset, topOffset)

background :: Color
background = black

trussColor :: Color
trussColor = dark azure

-- Animation creation

start_animation :: IO ()
start_animation = animate window background createTruss

createTruss :: Float -> Picture
createTruss time = Graphics.Gloss.scale Animator.scale Animator.scale (pictures (map (createLine time) (M.toList Constructor.edges)))

createLine :: Float -> Edge -> Picture
createLine time (_, (a, b)) = color trussColor $ line [getPointLoc time a, getPointLoc time b]

getPointLoc :: Float -> Int -> Point
getPointLoc time id = (((fst (M.lookupDefault (0,0) id Animator.nodes)) + ((fst (M.lookupDefault (0,0) id xyOffsets))*(amplitude * (sin (speed * time))))),((snd (M.lookupDefault (0,0) id Animator.nodes)) + ((snd (M.lookupDefault (0,0) id xyOffsets))*(amplitude * (sin (speed * time))))))