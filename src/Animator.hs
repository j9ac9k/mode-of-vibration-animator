module Animator ( start_animation ) where

import qualified Data.HashMap.Lazy as M
import GHC.Float (double2Float, float2Int, int2Float)
import Graphics.Gloss
import Constructor
import Numeric

type Node       = (Int, (Float, Float))
type Edge       = (Int, (Int, Int))
type XYoffset   = (Int, (Float, Float))

to_animate_float :: [Float]
to_animate_float =  [double2Float value | value <- Constructor.to_animate]

nodes = M.fromList [(id, (double2Float x, double2Float y)) | (id, (x, y)) <- M.toList Constructor.nodes]


xyOffsets = M.fromList (makeOffsets to_animate_float 1)

makeOffsets :: [Float] -> Int -> [(Int, (Float, Float))]
makeOffsets [] _        = []
makeOffsets (x1:x2:xs) i    = (i,(x1, x2)) : makeOffsets xs (i+1)

-- Display parameters

width, height, winLoc :: Int
width       = 800
height      = 800
winLoc      = 400
leftShift   = (maxX + minX) / 2 :: Float
topShift    = (maxY + minY) / 2 :: Float
speed       = 3 :: Float
amplitude   = 10 :: Float
scale       = int2Float (width `div` ((float2Int maxXY) + 5))

minX        = minimum (map fst (map snd (M.toList Animator.nodes)))
minY        = minimum (map snd (map snd (M.toList Animator.nodes)))
maxX        = maximum (map fst (map snd (M.toList Animator.nodes)))
maxY        = maximum (map snd (map snd (M.toList Animator.nodes)))
maxXY       = maximum [(maxX - minX), (maxY - minY)]

window :: Display
window = InWindow "Vibration Display" (width, height) (winLoc, winLoc)

background :: Color
background = light black

trussColor :: Color
trussColor = azure

freqColor :: Color
freqColor = orange

-- Animation creation

start_animation :: IO ()
start_animation = animate window background createTruss

createTruss :: Float -> Picture
createTruss time = Graphics.Gloss.scale Animator.scale Animator.scale (pictures (createPictures time))

createPictures :: Float -> [Picture]
createPictures time = (map (createLine time) (M.toList Constructor.edges)) ++ [createText]

createText :: Picture
createText = color freqColor $ Graphics.Gloss.translate (0 - (int2Float (width `div` float2Int(2 * Animator.scale)))) ((int2Float (height `div` float2Int(2 * Animator.scale))) - 4) (Graphics.Gloss.scale (1/100) (1/100) (text ("Freq: " ++ Numeric.showFFloat (Just 2) Constructor.freq "" ++ " Hz")))

createLine :: Float -> Edge -> Picture
createLine time (_, (a, b)) = color trussColor $ line [getPointLoc time a, getPointLoc time b]

getPointLoc :: Float -> Int -> Point
getPointLoc time id = ((((fst (M.lookupDefault (0, 0) id Animator.nodes)) + ((fst (M.lookupDefault (0, 0) id xyOffsets)) * (amplitude * (sin (speed * time))))) - leftShift), (((snd (M.lookupDefault (0, 0) id Animator.nodes)) + ((snd (M.lookupDefault (0, 0) id xyOffsets)) * (amplitude * (sin (speed * time))))) - topShift))
