module Stats.Types where

import Data.Vector.Unboxed (Unbox)
import Linear


class ( Foldable v, Applicative v, Additive v, Trace v, Traversable v )
      => VectorSpace v where


instance VectorSpace V0
instance VectorSpace V1


-- | LinAlg represents a vector type we can do linear algebra in
class ( VectorSpace v
      , Floating a, Floating (v a)
      , Unbox a, Unbox (v a) )
      => LinAlg v a where


instance LinAlg V0 Double where

instance LinAlg V1 Double where