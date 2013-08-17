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

  -- * Frame creation
  makePicture,
  makeField,

) where

-- Friends
import Graphics.Gloss.Accelerate.Render
import Graphics.Gloss.Accelerate.Data.Color
import Graphics.Gloss.Accelerate.Data.Point
import Graphics.Gloss.Accelerate.Data.Picture

-- Standard library
import Prelude                                          as P

-- Gloss
import Graphics.Gloss.Data.Display                      ( Display(..) )
import Graphics.Gloss.Data.Picture                      ( Picture(..) )
import Graphics.Gloss.Interface.IO.Animate              as G ( animateFixedIO, black )
import Graphics.Gloss.Interface.Pure.Game               as G ( Event, play )

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
animateFieldWith render display (zoomX, zoomY) makePixel
  | zoomX < 1 || zoomY < 1
  = error "Graphics.Gloss.Raster.Field: invalid pixel scalar factor"

  | otherwise
  = let
        -- Size of the raw image to render
        (winSizeX, winSizeY)    = sizeOfDisplay display

        picture time            = makePicture render winSizeX winSizeY zoomX zoomY (\t -> makePixel (the t))
                                $ fromList Z [time]
    in
    animateFixedIO display G.black (return . picture)


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
playFieldWith render display (zoomX, zoomY) stepRate
              initState makeWorld makePixel handleEvent stepState
  | zoomX < 1 || zoomY < 1
  = error "Graphics.Gloss.Raster.Field: invalid pixel scalar factor"

  | otherwise
  = let
        -- Size of the raw image to render
        (winSizeX, winSizeY)    = sizeOfDisplay display

        picture                 = makePicture render winSizeX winSizeY zoomX zoomY makePixel
                                . makeWorld
    in
    play display G.black stepRate initState picture handleEvent stepState


-- Internals
-- ---------

sizeOfDisplay :: Display -> (Int, Int)
sizeOfDisplay display
  = case display of
      InWindow _ s _    -> s
      FullScreen s      -> s

makePicture
    :: Arrays world
    => Render                                   -- ^ method to compute the field
    -> Int                                      -- ^ window width
    -> Int                                      -- ^ window height
    -> Int                                      -- ^ pixel width
    -> Int                                      -- ^ pixel height
    -> (Acc world -> Exp Point -> Exp Color)    -- ^ function to apply at each point
    -> (world -> Picture)                       -- ^ new function that generates the picture
makePicture render winSizeX winSizeY zoomX zoomY makePixel
  = let -- Size of the raw image to render
        sizeX           = winSizeX `div` zoomX
        sizeY           = winSizeY `div` zoomY

        -- Compute the image
        pixels          = makeField render sizeX sizeY makePixel

        -- Turn the array into a Gloss picture
        picture world   = bitmapOfArray (pixels world) False
    in
    Scale (P.fromIntegral zoomX) (P.fromIntegral zoomY) . picture

makeField
    :: Arrays world
    => Render                                   -- ^ method to compute the field
    -> Int                                      -- ^ width
    -> Int                                      -- ^ height
    -> (Acc world -> Exp Point -> Exp Color)    -- ^ function to apply at each point
    -> (world -> Array DIM2 Word32)             -- ^ new function that computes the image
makeField render sizeX sizeY makePixel
  = render
  $ \world -> A.generate (constant (Z :. sizeY :. sizeX))
                         (packRGBA . opaque . makePixel world . pointOfIndex sizeX sizeY)

