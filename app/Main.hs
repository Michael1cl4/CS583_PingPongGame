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
  , sceneState :: Int                -- 0: Instruction, 1: Play, 2: End
--  , ballspeed :: Float
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
  , sceneState = 0
--  , ballspeed = 10
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
render game = case (sceneState game) of
                0 -> pictures [instruction1, instruction2, next, welcome]
                1 -> pictures [ball, walls, mkBat rose bat1x (bat1 game), mkBat orange bat2x (bat2 game),player1_score, colon, player2_score]
                2 -> pictures [endTitle, endSubtitle, endEdit1, endEdit2, endEdit3]
  where
    -- Instruction Scene
    welcome = translate (-185) 90 (scale 0.2 0.2 (text "Welcome to PingPong Game"))
    instruction1 = translate (-180) (-60) (scale 0.12 0.12 (text "Player1 use PgUp/PgDn to control the left bat"))
    instruction2 = translate (-165) (-80) (scale 0.12 0.12 (text "Player2 use W/S to control the right bat"))
    next  = translate (-110) (-170) (scale 0.2 0.2 (text "Press Q to play"))
    -- End Scene
    endTitle    = translate (-150) 100   (scale 0.4 0.4 (text ("Player"++ show(1) ++" Win!!!")))
    endSubtitle = translate (-110) 50    (scale 0.2 0.2 (text "[Game Developers]"))
    endEdit1  = translate (-110) 20    (scale 0.1 0.1 (text "Yinchao Zhu zhuyin@oregonstate.edu"))
    endEdit2  = translate (-110) 0     (scale 0.1 0.1 (text "Haoyuan Qiu iuha@oregonstate.edu"))
    endEdit3  = translate (-110) (-20) (scale 0.1 0.1 (text "Shukan Nieh niehsh@oregonstat.edu"))
    -- the current score
    player1_score = translate 60 165 (scale 0.2 0.2 (text (show(5))))
    colon = translate 0 165 (scale 0.2 0.2 (text (":")))
    player2_score = translate (-60) 165 (scale 0.2 0.2 (text (show(7))))
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
movement seconds game = if (sceneState game) == 1 
                        then
                        game { ballPos = (x', y'), bat1 = y'', bat2 = y'''}
                        else game
  where
    -- Old Positions and velocities
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
batBounce game = game { ballVel = (vx', vy')}
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10
    -- The old velocities.
    (vx, vy) = ballVel game

    (vx', vy') = if batCollision game radius
          then
            -- Update the velocity.
            (-vx, vy)
          else
            -- Do nothing. Return the old velocity.
            (vx, vy)

-- | Given position and radius of the ball, return whether a collision occurred.
batCollision :: PPG -> Radius -> Bool
batCollision game radius = (leftXRange (ballPos game) (bat2 game) || rightXRange (ballPos game) (bat1 game))
  where
    leftXRange (x,ball_y) bat_y = (x - radius == bat2x + bat_width / 2)
                               && ((ball_y <= bat_height / 2 + bat_y) && (ball_y >= -bat_height / 2 + bat_y))
    rightXRange (x,ball_y) bat_y = (x + radius ==  bat1x - bat_width / 2)
                               && ((ball_y <= bat_height / 2 + bat_y) && (ball_y >= -bat_height / 2 + bat_y))

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

-- | Judge Win/Lose
outofBound :: PPG -> PPG
outofBound game = if leftout (ballPos game) || rightout (ballPos game) 
                  then game {sceneState = 2}
                  else game
  where
    radius = 10
    leftout (ball_x, _) = ball_x - radius <= -boundary_width /2
    rightout (ball_x, _) = ball_x + radius >= boundary_width /2



-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> PPG -> PPG
update seconds = outofBound . batBounce . wallBounce . movement seconds

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

-- Out of the End Scene
handleKeys (EventKey (Char 'q') Down _ _) game = case (sceneState game) of
                                                 0 -> game {sceneState = 1}
                                                 1 -> game {sceneState = 2}
                                                 2 -> initialState

-- Do nothing for all other events.
handleKeys _ game = game

