{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Graphics.Gloss.Accelerate.Render
-- Copyright   : [2013..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

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

