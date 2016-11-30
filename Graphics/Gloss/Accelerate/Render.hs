{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

module Graphics.Gloss.Accelerate.Render
  where

import Data.Array.Accelerate


-- | The type for executing Accelerate computations. This matches the 'run1'
--   style of executing programs.
--
--   Some variants of the display functions take an argument of this type, which
--   determine how computations are executed.
--
type Render = forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b

