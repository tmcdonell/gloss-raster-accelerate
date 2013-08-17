{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

module Graphics.Gloss.Accelerate.Render
  where

import Data.Array.Accelerate                            ( Arrays, Acc )

#ifdef ACCELERATE_CUDA_BACKEND
import Data.Array.Accelerate.CUDA
#else
import Data.Array.Accelerate.Interpreter
#endif


-- | The type of executing Accelerate computations. Some variants of the display
--   functions take an argument of this type, which determine how computations
--   are executed.
--
type Render = forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b


-- | The set of backends available to execute Accelerate programs. The default
--   chooses the fastest available, which is currently the CUDA backend. This is
--   controlled via the import statements.
--
defaultRender :: Render
defaultRender =  run1

