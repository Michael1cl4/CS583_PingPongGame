module Main where

-- To check the Color setting:
-- https://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Data-Color.html#t:Color

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort

-- | The size of our playground
boundary_width, boundary_height, boundary_offset :: Float
boundary_width = 300
boundary_height = 300
boundary_offset = 100

-- | The size of the bat
bat_width, bat_height :: Float
bat_width  = 20
bat_height = 80

-- | The bat position on X-axis
bat1x, bat2x :: Float
bat1x = 120
bat2x = -120

-- | The size of the wall
wall_width, wall_height  :: Float
wall_width = 270
wall_height = 10

-- | Ping Pong Game State 
data PPG = Game
  { ballPos :: (Float, Float)    -- ^ Pong ball (x, y) Position.
  , ballVel :: (Float, Float)     -- ^ Pong ball (x, y) Velocity. 
  , bat1 :: Float                     -- ^ Left player bat height.
                                            -- Zero is the middle of the screen. 
  , bat2 :: Float                      -- ^ Right player bat height.
  , bat1state :: Int                 -- 0: stop, 1: move up, 2: move down
  , bat2state :: Int                 -- 0: stop, 1: move up, 2: move down
  } deriving Show

-- | The Initial State of the PPG
initialState :: PPG
initialState = Game
  { ballPos = (-10, 30)
  , ballVel = (-30, -40)
  , bat1 = 40
  , bat2 = -80
  , bat1state = 0
  , bat2state = 0
  }

-- | For Reading the function much easier 
type Radius = Float 
type Position = (Float, Float)

-- | Define the window size and name
window :: Display
window = InWindow "Ping Pong Game" (400, 400) (0, 0)

-- | For the different Scene, the background color could be different
background_Color, wall_Color, bat_Color, ball_Color  :: Color
background_Color = white
wall_Color = greyN 0.5
bat_Color = light blue
ball_Color = dark red

{-
play:: 
Display	                        Display mode.
-> Color	                    Background color.
-> Int	                        Number of simulation steps to take for each second of real time.
-> world	                    The initial world.
-> (world -> Picture)	        A function to convert the world a picture.
-> (Event -> world -> world)	A function to handle input events.
-> (Float -> world -> world)	A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.
-> IO ()	 
-}

main :: IO ()
main = play window background_Color 60 initialState render handleKeys update


-- | Convert a game state into a picture.
render :: PPG  -> Picture
render game =
  pictures [ball, walls,
            mkBat rose bat1x (bat1 game),
            mkBat orange bat2x (bat2 game)]
  where
    --  The pong ball.
    ball = uncurry translate (ballPos game) ( color ball_Color  (circleSolid 10))

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall boundary_offset =
      translate 0 boundary_offset (color wall_Color (rectangleSolid wall_width wall_height))

    walls = pictures [wall (boundary_height / 2), wall (boundary_height / 2 * (-1))]

    --  Make a bat of a given border and vertical offset.
    mkBat :: Color -> Float -> Float -> Picture
    mkBat col x y = pictures [translate x y (color bat_Color (rectangleSolid bat_width bat_height))]

-- | Update the ball position using its current velocity.
movement :: Float -> PPG -> PPG

movement seconds game = game { ballPos = (x', y'), bat1 = y'', bat2 = y'''}
  where
    -- Old Positions and velocities.
    (x, y) = ballPos game
    (vx, vy) = ballVel game

    -- New Positions.
    x' = x + vx * seconds
    y' = y + vy * seconds

    -- New Position of bat
    y'' = case (bat1state game) of
            0 -> (bat1 game)
            1 -> (bat1 game) + 10
            2 -> (bat1 game) - 10
    y''' = case (bat2state game) of
            0 -> (bat2 game)
            1 -> (bat2 game) + 10
            2 -> (bat2 game) - 10


-- | Detect a collision with a bat. Upon collisions,
-- change the velocity of the ball to bounce it off the bat.
batBounce :: PPG -> PPG
batBounce game = game { ballVel = (vx', vy)}
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10
    -- The old velocities.
    (vx, vy) = ballVel game

    vx' = if batCollision (ballPos game) radius
          then
            -- Update the velocity.
            -vx
          else
            -- Do nothing. Return the old velocity.
            vx

-- | Given position and radius of the ball, return whether a collision occurred.
batCollision :: Position -> Radius -> Bool 
batCollision (x, _) radius = leftCollision || rightCollision
  where
    leftCollision  = x - radius <=  -boundary_width / 2 
    rightCollision = x + radius >=  boundary_width / 2 


-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
wallBounce :: PPG -> PPG
wallBounce game = game { ballVel = (vx, vy'), bat1 = y'' , bat2 = y''' }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10
    -- The old velocities.
    (vx, vy) = ballVel game

    vy' = if wallCollision (ballPos game) radius
          then
             -- Update the velocity.
             -vy
           else
            -- Do nothing. Return the old velocity.
            vy

    p1y = bat1 game
    y'' = if p1y >=  (boundary_height - bat_height) / 2
          then
            (boundary_height - bat_height) / 2
          else
            if p1y <=  -(boundary_height - bat_height) / 2
            then
              -(boundary_height - bat_height) / 2
            else
              p1y

    p2y = bat2 game
    y''' = if p2y >=  (boundary_height - bat_height) / 2
          then
            (boundary_height - bat_height) / 2
          else
            if p2y <=  -(boundary_height - bat_height) / 2
            then
              -(boundary_height - bat_height) / 2
            else
              p2y

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool 
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= - boundary_height / 2 
    bottomCollision = y + radius >=  boundary_height / 2

-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> PPG -> PPG
update seconds = batBounce . wallBounce . movement seconds


-- | Respond to key events.
handleKeys :: Event -> PPG -> PPG

-- For an 'space' keypress, reset the ball.
handleKeys (EventKey (SpecialKey KeySpace) _ _ _) game =  
  game { ballPos = (0, 0) }

-- For an KeyUp keypress, move up the bat1.
handleKeys (EventKey (Char 'w') Down _ _) game = game {bat1state = 1}
-- For an KeyDown keypress, move down the bat1.
handleKeys (EventKey (Char 's') Down _ _) game = game {bat1state = 2}
-- For an KeyPageUp keypress, move up the bat2.
handleKeys (EventKey (SpecialKey KeyPageUp) Down _ _) game = game {bat2state = 1}
-- For an KeyPageDown keypress, move down the bat2.
handleKeys (EventKey (SpecialKey KeyPageDown) Down _ _) game = game {bat2state = 2}

handleKeys (EventKey (Char 's') Up _ _) game = game {bat1state = 0}
handleKeys (EventKey (Char 'w') Up _ _) game = game {bat1state = 0}
handleKeys (EventKey (SpecialKey KeyPageUp) Up _ _) game = game {bat2state = 0}
handleKeys (EventKey (SpecialKey KeyPageDown) Up _ _) game = game {bat2state = 0}
-- Do nothing for all other events.
handleKeys _ game = game

