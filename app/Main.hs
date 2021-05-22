module Main where

-- To check the Color setting:
-- https://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Data-Color.html#t:Color

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
import System.Random
import System.IO.Unsafe

-- | The size of our playground
boundary_width, boundary_height, boundary_offset :: Float
boundary_width = 300
boundary_height = 300
boundary_offset = 100

-- | The size of the bat
bat_width :: Float
bat_width  = 20

-- | The bat position on X-axis
bat1x, bat2x :: Float
bat1x = 120
bat2x = -120

-- | The size of the wall
wall_width, wall_height  :: Float
wall_width = 270
wall_height = 10

win_score :: Int
win_score = 5

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
  , ballspeed :: Float
  , p1score :: Int
  , p2score :: Int
  , bat1_height :: Float
  , bat2_height :: Float
  , ai_mod :: Int
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
  , ballspeed = 1.2
  , p1score = 0
  , p2score = 0
  , bat1_height = 80
  , bat2_height = 80
  , ai_mod = 0
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
main = play window background_Color 120 initialState render handleKeys update


-- | Convert a game state into a picture.
render :: PPG  -> Picture
render game = case (sceneState game) of
                0 -> if  (ai_mod game == 1) then
                          pictures [ai , instruction2, instruction2_diff, bat2_shape (bat2_height game), next, welcome, mod]
                       else
                          pictures [instruction1, instruction1_diff, bat1_shape (bat1_height game), instruction2, instruction2_diff, bat2_shape (bat2_height game), next, welcome, mod]
                1 -> pictures [ball, walls, mkBat rose bat1x (bat1 game) (bat1_height game), mkBat orange bat2x (bat2 game) (bat2_height game),player1_score, colon, player2_score]
                2 -> pictures [endTitle, endSubtitle, endEdit1, endEdit2, endEdit3]
  where
    -- Instruction Scene
    welcome = translate (-185) 110 (scale 0.2 0.2 (text "Welcome to PingPong Game"))
    instruction1 = translate (-190) (60) (scale 0.12 0.12 (text "Player1 use PgUp/PgDn to control the right bat"))
    instruction1_diff = translate (-180) (30) (scale 0.12 0.12 (text "Player1 use '1' to tune the bat length"))
    ai = translate (-100) (40) (scale 0.12 0.12 (text "Player1 is controlled by AI"))
    bat1_shape bat1_height = translate (0) (0) (color bat_Color (rectangleSolid bat1_height (10)))
    instruction2 = translate (-180) (-30) (scale 0.12 0.12 (text "Player2 use W/S to control the left bat"))
    instruction2_diff = translate (-180) (-60) (scale 0.12 0.12 (text "Player2 use '2' to tune the bat length"))
    bat2_shape bat2_height = translate (0) (-90) (color bat_Color (rectangleSolid bat2_height (10)))
    next  = translate (-110) (-150) (scale 0.2 0.2 (text "Press Q to play"))
    mod  = translate (-170) (-190) (scale 0.2 0.2 (text "Press M to change mode"))
    -- End Scene
    endTitle    = if (p1score game == win_score)
                       then
                              translate (-150) 100   (scale 0.4 0.4 (text ("Player"++ show(1) ++" Win!!!")))
                       else translate (-150) 100   (scale 0.4 0.4 (text ("Player"++ show(2) ++" Win!!!")))
    endSubtitle = translate (-110) 50    (scale 0.2 0.2 (text "[Game Developers]"))
    endEdit1  = translate (-110) 20    (scale 0.1 0.1 (text "Yinchao Zhu zhuyin@oregonstate.edu"))
    endEdit2  = translate (-110) 0     (scale 0.1 0.1 (text "Haoyuan Qiu iuha@oregonstate.edu"))
    endEdit3  = translate (-110) (-20) (scale 0.1 0.1 (text "Shukan Nieh niehsh@oregonstat.edu"))
    -- the current score
    player1_score = translate 60 165 (scale 0.2 0.2 (text (show(p1score game))))
    colon = translate 0 165 (scale 0.2 0.2 (text (":")))
    player2_score = translate (-60) 165 (scale 0.2 0.2 (text (show(p2score game))))
    --  The pong ball.
    ball = uncurry translate (ballPos game) ( color ball_Color  (circleSolid 10))

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall boundary_offset =
      translate 0 boundary_offset (color wall_Color (rectangleSolid wall_width wall_height))

    walls = pictures [wall (boundary_height / 2), wall (boundary_height / 2 * (-1))]

    --  Make a bat of a given border and vertical offset.
    mkBat :: Color -> Float -> Float -> Float -> Picture
    mkBat col x y h = pictures [translate x y (color bat_Color (rectangleSolid bat_width h))]

-- | Update the ball position using its current velocity.
movement :: Float -> PPG -> PPG
movement seconds game = if (sceneState game) == 1
                        then game { ballPos = (x', y'), bat1 = y'', bat2 = y'''}
                        else game
  where
    -- Old Positions and velocities
    (x, y) = ballPos game
    (vx, vy) = ballVel game
     
    -- New Positions.
    x' = x + vx * seconds
    y' = y + vy * seconds

    -- New Position of bat
    x1 = unsafePerformIO (getStdRandom (randomR (0.5, 1.5)))
    y'' = if (ai_mod game == 0) then
                case (bat1state game) of
                0 -> (bat1 game)
                1 -> (bat1 game) + 5
                2 -> (bat1 game) - 5
             else
               if y > (bat1 game) 
               then (bat1 game) + x1
               else (bat1 game) - x1
    y''' = case (bat2state game) of
            0 -> (bat2 game)
            1 -> (bat2 game) + 5
            2 -> (bat2 game) - 5


-- | Detect a collision with a bat. Upon collisions,
-- change the velocity of the ball to bounce it off the bat.
batBounce :: PPG -> PPG
batBounce game = game {ballVel = (vx', vy')}
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10
    -- The old velocities.
    (vx, vy) = ballVel game
 --   (x, y) = ballPos game

    (vx', vy') = if batCollision game radius
          then
            -- Update the velocity.
            if (abs(vx) * (ballspeed game) < 150 && abs(vy) * (ballspeed game) < 200 )
            then
            (-vx * (ballspeed game), vy * (ballspeed game))
            else
            (-vx, vy)
          else
            -- Do nothing. Return the old velocity.
            (vx, vy)

-- | Given position and radius of the ball, return whether a collision occurred.
batCollision :: PPG -> Radius -> Bool
batCollision game radius = (leftXRange (ballPos game) (bat2 game) (bat2_height game) || rightXRange (ballPos game) (bat1 game) (bat1_height game))
  where
    leftXRange (x,ball_y) bat_y bat_h = ( floor(x - radius) == floor(bat2x + bat_width / 2))
                               && ((ball_y <= bat_h / 2 + bat_y) && (ball_y >= -bat_h / 2 + bat_y))
    rightXRange (x,ball_y) bat_y bat_h = (floor(x + radius) ==  floor(bat1x - bat_width / 2))
                               && ((ball_y <= bat_h / 2 + bat_y) && (ball_y >= -bat_h / 2 + bat_y))

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
    y'' = if p1y >=  (boundary_height - (bat1_height game)) / 2
          then
            (boundary_height - (bat1_height game)) / 2
          else
            if p1y <=  -(boundary_height - (bat1_height game)) / 2
            then
              -(boundary_height - (bat1_height game)) / 2
            else
              p1y

    p2y = bat2 game
    y''' = if p2y >=  (boundary_height - (bat2_height game)) / 2
          then
            (boundary_height - (bat2_height game)) / 2
          else
            if p2y <=  -(boundary_height - (bat2_height game)) / 2
            then
              -(boundary_height - (bat2_height game)) / 2
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
outofBound game = if leftout (ballPos game)
                  then game {p1score = (p1score game) + 1, ballPos = (0, 0), ballVel = (-30, -40) }
                  else if rightout (ballPos game)
      then game {p2score = (p2score game) + 1, ballPos = (0, 0), ballVel = (-30, -40)}
      else game
  where
    radius = 10
    leftout (ball_x, _) = ball_x - radius <= -boundary_width /2
    rightout (ball_x, _) = ball_x + radius >= boundary_width /2

finishCheck :: PPG -> PPG
finishCheck game = if  (p2score game) == win_score || (p1score game) == win_score
                                 then
                                   game {sceneState = 2}
                                 else
                                   game            

-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> PPG -> PPG
update seconds = finishCheck . outofBound . batBounce . wallBounce . movement seconds

-- | Respond to key events.
handleKeys :: Event -> PPG -> PPG

-- For an 'space' keypress, reset the ball.
handleKeys (EventKey (SpecialKey KeySpace) _ _ _) game =  
  game { ballPos = (0, 0) }

-- For an KeyUp keypress, move up the bat1.
handleKeys (EventKey (Char 'w') Down _ _) game = game {bat2state = 1}
-- For an KeyDown keypress, move down the bat1.
handleKeys (EventKey (Char 's') Down _ _) game = game {bat2state = 2}
-- For an KeyPageUp keypress, move up the bat2.
handleKeys (EventKey (SpecialKey KeyPageUp) Down _ _) game = game {bat1state = 1}
-- For an KeyPageDown keypress, move down the bat2.
handleKeys (EventKey (SpecialKey KeyPageDown) Down _ _) game = game {bat1state = 2}

-- For an “1” keypress, move up the bat1.
handleKeys (EventKey (Char '1') Down _ _) game = if ( (sceneState game) == 0) then
                                                                                      case (bat1_height game) of
                                                                                         40 -> game {bat1_height = 60}
                                                                                         60 -> game {bat1_height = 80}
                                                                                         80 -> game {bat1_height = 100}
                                                                                         100 -> game {bat1_height = 120}
                                                                                         120 -> game {bat1_height = 40}
                                                                                   else
                                                                                      game
-- For an “2” keypress, move down the bat1.
handleKeys (EventKey (Char '2') Down _ _) game = if ( (sceneState game) == 0) then
                                                                                      case (bat2_height game) of
                                                                                         40 -> game {bat2_height = 60}
                                                                                         60 -> game {bat2_height = 80}
                                                                                         80 -> game {bat2_height = 100}
                                                                                         100 -> game {bat2_height = 120}
                                                                                         120 -> game {bat2_height = 40}
                                                                                   else
                                                                                      game

handleKeys (EventKey (Char 's') Up _ _) game = game {bat2state = 0}
handleKeys (EventKey (Char 'w') Up _ _) game = game {bat2state = 0}
handleKeys (EventKey (SpecialKey KeyPageUp) Up _ _) game = if ((ai_mod game) == 0) then game {bat1state = 0}
                                                           else game
handleKeys (EventKey (SpecialKey KeyPageDown) Up _ _) game = if ((ai_mod game) == 0) then game {bat1state = 0}
                                                             else game

handleKeys (EventKey (Char 'm') Down _ _) game = case (ai_mod game) of
                                                                                    0 -> game {ai_mod = 1}
                                                                                    1 -> game {ai_mod = 0}

-- Out of the End Scene
handleKeys (EventKey (Char 'q') Down _ _) game = case (sceneState game) of
                                                 0 -> game {sceneState = 1}
                                                 1 -> initialState
                                                 2 -> initialState

-- Do nothing for all other events.
handleKeys _ game = game
