module Main where

import Stats.Types
import MLCore

import Linear
import qualified Data.Vector.Unboxed as VU
import Text.Printf


main :: IO ()
main = do
  let
    trainSet :: [(V1 Double, VU.Vector (V1 Double))]
    trainSet = [(V1 0, VU.fromList [V1 0])]
    fitted   = fitModel2 trainSet :: VU.Vector (V0 Double)
  printf "results: %s\n" (show $ VU.toList fitted)