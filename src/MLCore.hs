module MLCore where

import Stats.Types

import Linear
import Control.Monad.State.Strict
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as G
import Data.Vector.Unboxed (Unbox)
import Data.Vector.Unboxed.Deriving (derivingUnbox)


type MLModel vp vs =
  ( LinAlg vp Double, LinAlg vs Double )

data MLConfig vp vs where
  MLConfig :: MLModel vp vs => MLConfig vp vs

derivingUnbox "MLConfig"
  [t| forall vp vs. MLModel vp vs => MLConfig vp vs -> () |]
  [| \(MLConfig) -> () |]
  [| \() -> MLConfig |]



calc_zs :: ( LinAlg s Double
           , G.Vector vec (s Double) )
        => vec (s Double) -> State Int (vec (s Double))
calc_zs ys = do
  (G.mapM . mapM) (const $ return 0) ys
{-# INLINE calc_zs #-}



calc_sums :: forall vp vs. MLModel vp vs
            => VU.Vector (vp Double)
            -> VU.Vector (vp Double)
            -> VU.Vector (vs Double)
calc_sums xs zs =
  let n = G.length xs
  in VU.replicate (n + 1) (pure 0)
{-# INLINE calc_sums #-}



guessStates' :: forall vp vs.  MLModel vp vs => MLConfig vp vs -> VU.Vector (vs Double)
guessStates' params =
  let
    n = 10
    xs = G.replicate n (pure 0)
    ys = VU.replicate n (pure 0 :: vp Double)
  in flip evalState 0 $ do
    zs <- calc_zs ys
    return $ calc_sums xs zs
{-# INLINE guessStates' #-}



guessStates :: MLModel V1 vs => MLConfig V1 vs -> VU.Vector (vs Double)
guessStates params =
  guessStates' params -- needed to trigger bug!
{-# INLINE guessStates #-}



class ModelObs obs vs where
  fitModel :: MLModel V1 vs => [obs] -> VU.Vector (vs Double)


instance (MLModel V1 vs) => ModelObs (V1 Double, VU.Vector (V1 Double)) vs where
  fitModel allRsTL =
    guessStates MLConfig
  {-# INLINE fitModel #-}



fitModel2 :: (MLModel V1 vs, ModelObs obs vs) => [obs] -> VU.Vector (vs Double)
fitModel2 obsTL = -- this is needed to trigger bug! If Main.hs calls `fitModel` directly <<loop>> does not happen.
  fitModel obsTL
{-# NOINLINE fitModel2 #-}