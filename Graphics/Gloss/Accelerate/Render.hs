{-# LANGUAGE CPP #-}

-- | Accelerate backends we know about.
--
module Graphics.Gloss.Accelerate.Render
  where

import Data.Array.Accelerate                            ( Arrays, Acc )
import qualified Data.Array.Accelerate.Interpreter      as Interp
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA             as CUDA
#endif


-- | The set of backends available to execute Accelerate programs. The base
--   functions choose `maxBound' as the default, so there should be some honesty
--   in how this list is sorted.
--
data Render = Interpreter
#ifdef ACCELERATE_CUDA_BACKEND
            | CUDA
#endif
  deriving (Eq, Show, Bounded)


-- | Execute Accelerate programs
--
run :: Arrays a => Render -> Acc a -> a
run Interpreter = Interp.run
#ifdef ACCELERATE_CUDA_BACKEND
run CUDA        = CUDA.run
#endif


-- | Execute an Accelerate program of one argument
--
run1 :: (Arrays a, Arrays b) => Render -> (Acc a -> Acc b) -> a -> b
run1 Interpreter f = Interp.run1 f
#ifdef ACCELERATE_CUDA_BACKEND
run1 CUDA        f = CUDA.run1 f
#endif


-- | The list of available renderers with a short descriptive string
--
availableRenderers :: [(Render, String)]
availableRenderers
  = [ ( Interpreter,    "reference implementation (sequential)" )
#ifdef ACCELERATE_CUDA_BACKEND
    , ( CUDA,           "implementation for NVIDIA GPUs (parallel)" )
#endif
    ]

