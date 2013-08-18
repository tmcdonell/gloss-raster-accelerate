{-# LANGUAGE RankNTypes #-}

-- | Rendering of 2D functions as raster fields
--
module Graphics.Gloss.Accelerate.Raster.Field (

  module Graphics.Gloss.Accelerate.Data.Point,
  module Graphics.Gloss.Accelerate.Data.Color,

  -- * Display functions
  Render, Display(..),
  animateField, animateFieldWith,
  playField, playFieldWith,

  -- * Field creation
  makeField,

) where

-- Friends
import Graphics.Gloss.Accelerate.Render
import Graphics.Gloss.Accelerate.Data.Color
import Graphics.Gloss.Accelerate.Data.Point
import Graphics.Gloss.Accelerate.Raster.Array

-- Standard library
import Prelude                                          as P

-- Gloss
import Graphics.Gloss.Interface.Pure.Game               ( Event )

-- Accelerate
import Data.Array.Accelerate                            as A


-- Animate --------------------------------------------------------------------
-- -------

-- | Animate a continuous 2D function using the default backend
--
animateField
    :: Display                          -- ^ Display mode
    -> (Int, Int)                       -- ^ Number of pixels to draw per point
    -> (Exp Float -> Exp Point -> Exp Color)
            -- ^ A function to compute the colour at a particular point.
            --
            --   It is passed the time in seconds since the program started, and
            --   a point between (-1,1) and (+1,1).
    -> IO ()
animateField = animateFieldWith defaultRender


-- | Animate a continuous 2D function, specifying the backend used to render
--   the field.
--
animateFieldWith
    :: Render           -- ^ Method to render the field
    -> Display          -- ^ Display mode
    -> (Int, Int)       -- ^ Number of pixels to draw per point
    -> (Exp Float -> Exp Point -> Exp Color)
            -- ^ A function to compute the colour at a particular point.
            --
            --   It is passed the time in seconds since the program started, and
            --   a point between (-1,1) and (+1,1).
    -> IO ()
animateFieldWith render display zoom@(zoomX, zoomY) makePixel
  = let -- size of the window
        (winSizeX, winSizeY)    = sizeOfDisplay display

        -- size of the raw image to render
        sizeX                   = winSizeX `div` zoomX
        sizeY                   = winSizeY `div` zoomY
    in
    animateArrayWith
        render
        display
        zoom
        (makeField sizeX sizeY makePixel)


-- | Play a game with a continuous 2D function using the default backend.
--
playField
    :: Arrays world
    => Display          -- ^ Display mode
    -> (Int, Int)       -- ^ Number of pixels to draw per point
    -> Int              -- ^ Number of simulation steps to take for each second of real time
    -> state            -- ^ The initial state
    -> (state -> world) -- ^ Extract the world state
    -> (Acc world -> Exp Point -> Exp Color)
            -- ^ Compute the colour of the world at a given point
    -> (Event -> state -> state)
            -- ^ Handle input events
    -> (Float -> state -> state)
            -- ^ Step the world one iteration.
            --   It is passed the time in seconds since the program started.
    -> IO ()
playField = playFieldWith defaultRender


-- | Play a game with a continuous 2D function, specifying the method used to
--   render the field.
--
playFieldWith
    :: Arrays world
    => Render           -- ^ Method to render the field
    -> Display          -- ^ Display mode
    -> (Int, Int)       -- ^ Number of pixels to draw per point
    -> Int              -- ^ Number of simulation steps to take for each second of real time
    -> state            -- ^ The initial state
    -> (state -> world) -- ^ Extract the world state
    -> (Acc world -> Exp Point -> Exp Color)
            -- ^ Compute the colour of the world at a given point
    -> (Event -> state -> state)
            -- ^ Handle input events
    -> (Float -> state -> state)
            -- ^ Step the world one iteration.
            --   It is passed the time in seconds since the program started.
    -> IO ()
playFieldWith render display zoom@(zoomX, zoomY) stepRate
              initState makeWorld makePixel handleEvent stepState
  = let -- size of the window
        (winSizeX, winSizeY)    = sizeOfDisplay display

        -- size of the raw image to render
        sizeX                   = winSizeX `div` zoomX
        sizeY                   = winSizeY `div` zoomY
    in
    playArrayWith
        render
        display
        zoom
        stepRate
        initState
        makeWorld
        (makeField sizeX sizeY makePixel)
        handleEvent
        stepState


-- Internals
-- ---------

sizeOfDisplay :: Display -> (Int, Int)
sizeOfDisplay display
  = case display of
      InWindow _ s _    -> s
      FullScreen s      -> s


-- | Lift a point-wise colouring function into an image creation function.
--
--   The parameter 'world' at this point can be arbitrary. However if you use
--   this function standalone, you will probably at some point want the result
--   of this function to plug into 'makePicture' and thus 'Render', and thus be
--   a unary function from 'Arrays' to 'Arrays'.
--
makeField
    :: Int                                      -- ^ image width
    -> Int                                      -- ^ image height
    -> (world -> Exp Point -> Exp Color)        -- ^ function to apply at each point
    -> (world -> Acc (Array DIM2 Color))        -- ^ new function that generates the field
makeField sizeX sizeY makePixel world
  = A.generate (constant (Z :. sizeY :. sizeX))
               (makePixel world . pointOfIndex sizeX sizeY)

