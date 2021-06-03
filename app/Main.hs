module Main where

-- To check the Color setting:
-- https://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Data-Color.html#t:Color

import Graphics.Gloss
import System.Random
import System.IO.Unsafe
import Data
import HandleKey

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
    instruction1 = translate (-180) (60) (scale 0.12 0.12 (text "Player1 use O/L to control the right bat"))
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
    ball = uncurry translate (posx (ballStatus game), posy (ballStatus game)) ( color ball_Color  (circleSolid 10))

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall boundary_offset =
      translate 0 boundary_offset (color wall_Color (rectangleSolid wall_width wall_height))

    walls = pictures [wall (boundary_height / 2), wall (boundary_height / 2 * (-1))]

    --  Make a bat of a given border and vertical offset.
    mkBat :: Color -> Float -> Float -> Float -> Picture
    mkBat col x y h = pictures [translate x y (color bat_Color (rectangleSolid bat_width h))]

-- | Update the ball position using its current velocity.
moveball :: BS -> Float -> BS
moveball ballStatus seconds = ballStatus { posx = posx ballStatus + velx ballStatus * seconds
                                         , posy = posy ballStatus + vely ballStatus * seconds
                                         }

movement :: Float -> PPG -> PPG
movement seconds game = if (sceneState game) == 1
                        then game { ballStatus = moveball (ballStatus game) seconds
                                  , bat1 = y''
                                  , bat2 = y'''}
                        else game
  where
    -- New Position of bat
    x1 = unsafePerformIO (getStdRandom (randomR (0.5, 1.5)))
    y'' = if (ai_mod game == 0) then
                case (bat1state game) of
                0 -> (bat1 game)
                1 -> (bat1 game) + 5
                2 -> (bat1 game) - 5
             else
               if posy (ballStatus game) > (bat1 game) 
               then (bat1 game) + x1
               else (bat1 game) - x1
    y''' = case (bat2state game) of
            0 -> (bat2 game)
            1 -> (bat2 game) + 5
            2 -> (bat2 game) - 5


-- | Detect a collision with a bat. Upon collisions,
-- change the velocity of the ball to bounce it off the bat.
changeballvelbat :: BS -> Bool -> BS
changeballvelbat ballStatus True  = ballStatus { velx = -velx ballStatus * ballspeed ballStatus
                                            , vely = vely ballStatus * ballspeed ballStatus
                                            }
changeballvelbat ballStatus False = ballStatus {velx = -velx ballStatus}

batBounce :: PPG -> PPG
batBounce game = case batCollision game of
                 True  -> game{ballStatus = changeballvelbat (ballStatus game) (abs(velx (ballStatus game)) * ballspeed (ballStatus game) < 150 && abs(vely (ballStatus game)) * ballspeed (ballStatus game) < 200)
                              }
                 False -> game
{-
game {ballVel = (vx', vy')}
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10
    -- The old velocities.
    (vx, vy) = ballVel game
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
-}
-- | Given position and radius of the ball, return whether a collision occurred.
batCollision :: PPG -> Bool
batCollision game = (leftXRange (posx (ballStatus game)) (posy (ballStatus game)) (bat2 game) (bat2_height game)
                 || rightXRange (posx (ballStatus game)) (posy (ballStatus game)) (bat1 game) (bat1_height game))
  where
    leftXRange x ball_y bat_y bat_h = ( floor(x - ball_radius) == floor(bat2x + bat_width / 2))
                               && ((ball_y <= bat_h / 2 + bat_y) && (ball_y >= -bat_h / 2 + bat_y))
    rightXRange x ball_y bat_y bat_h = (floor(x + ball_radius) ==  floor(bat1x - bat_width / 2))
                               && ((ball_y <= bat_h / 2 + bat_y) && (ball_y >= -bat_h / 2 + bat_y))

-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
changeballvelwall :: BS -> Bool -> BS
changeballvelwall ballStatus True  = ballStatus {vely = -vely ballStatus}
changeballvelwall ballStatus False = ballStatus {vely = vely ballStatus}

wallBounce :: PPG -> PPG
wallBounce game = game { ballStatus = changeballvelwall (ballStatus game) (wallCollision (posy (ballStatus game)))
                       , bat1 = y'' 
                       , bat2 = y''' }
  where
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
wallCollision :: Float -> Bool 
wallCollision y = topCollision || bottomCollision
  where
    topCollision    = y - ball_radius <= - boundary_height / 2 
    bottomCollision = y + ball_radius >=  boundary_height / 2

-- | Judge Win/Lose
outofBound :: PPG -> PPG
outofBound game = if leftout (posx (ballStatus game))
                  then game {p1score = (p1score game) + 1, ballStatus = initballState }
                  else if rightout (posx (ballStatus game))
      then game {p2score = (p2score game) + 1, ballStatus = initballState}
      else game
  where
    leftout ball_x = ball_x - ball_radius <= -boundary_width /2
    rightout ball_x = ball_x + ball_radius >= boundary_width /2

finishCheck :: PPG -> PPG
finishCheck game = if  (p2score game) == win_score || (p1score game) == win_score
                                 then
                                   game {sceneState = 2}
                                 else
                                   game            

-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> PPG -> PPG
update seconds = finishCheck . outofBound . batBounce . wallBounce . movement seconds