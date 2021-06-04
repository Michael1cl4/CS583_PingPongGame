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
                Instruction WithAI -> pictures [ai , instruction2, instruction2_diff, bat2_shape (bat2_height game), next, welcome, mod]
                Instruction WithUser -> pictures [instruction1, instruction1_diff, bat1_shape (bat1_len (bat1Stat game)), instruction2, instruction2_diff, bat2_shape (bat2_height game), next, welcome, mod]
                Play _ -> pictures [ball, walls, mkBat rose bat1x (bat1 (bat1Stat game)) (bat1_len (bat1Stat game)), mkBat orange bat2x (bat2 game) (bat2_height game),player1_score, colon, player2_score]
                End -> pictures [endTitle, endSubtitle, endEdit1, endEdit2, endEdit3]
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
                  then translate (-150) 100   (scale 0.4 0.4 (text ("Player"++ show(1) ++" Win!!!")))
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
    ball = uncurry translate (posx (ballStat game), posy (ballStat game)) ( color ball_Color  (circleSolid ball_radius))

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
moveball ballStat seconds = ballStat { posx = posx ballStat + velx ballStat * seconds
                                     , posy = posy ballStat + vely ballStat * seconds}

movebat1 :: Bat1 -> BS -> Float -> PlayMod -> Bat1
movebat1 bat1Stat ballStat a WithUser = bat1Stat { bat1 = case motion bat1Stat of
                                                          BStop -> bat1 bat1Stat
                                                          BUp   -> bat1 bat1Stat + a
                                                          BDown -> bat1 bat1Stat - a }
movebat1 bat1Stat ballStat a WithAI   = bat1Stat { bat1 = if posy ballStat > bat1 bat1Stat
                                                          then bat1 bat1Stat + a
                                                          else bat1 bat1Stat - a}

movement :: Float -> PPG -> PPG
movement seconds game = case sceneState game of
                        Play WithUser -> game { ballStat = moveball (ballStat game) seconds
                                              , bat1Stat = movebat1 (bat1Stat game) (ballStat game) 5 WithUser
                                              , bat2 = y'''}
                        Play WithAI   -> game { ballStat = moveball (ballStat game) seconds
                                              , bat1Stat = movebat1 (bat1Stat game) (ballStat game) x1 WithAI
                                              , bat2 = y'''}
                        _             -> game
  where
    -- New Position of bat
    x1 = unsafePerformIO (getStdRandom (randomR (0.5, 1.5)))
    y''' = case (bat2state game) of
            0 -> (bat2 game)
            1 -> (bat2 game) + 5
            2 -> (bat2 game) - 5


-- | Detect a collision with a bat. Upon collisions,
-- change the velocity of the ball to bounce it off the bat.
chgBallVbat :: BS -> Bool -> BS
chgBallVbat ballStat True  = ballStat { velx = -velx ballStat * ballspeed ballStat
                                      , vely = vely ballStat  * ballspeed ballStat}
chgBallVbat ballStat False = ballStat {velx = -velx ballStat}

batBounce :: PPG -> PPG
batBounce game = case batCollision game of
                 True  -> game{ballStat = chgBallVbat (ballStat game) (abs(velx (ballStat game)) * ballspeed (ballStat game) < 150 && abs(vely (ballStat game)) * ballspeed (ballStat game) < 200)
                              }
                 False -> game
-- | Given position and radius of the ball, return whether a collision occurred.

batCollision :: PPG -> Bool
batCollision game = (leftXRange (posx (ballStat game)) (posy (ballStat game)) (bat2 game) (bat2_height game)
                 || rightXRange (posx (ballStat game)) (posy (ballStat game)) (bat1 (bat1Stat game)) (bat1_len (bat1Stat game)))
  where
    leftXRange x ball_y bat_y bat_h = ( floor(x - ball_radius) == floor(bat2x + bat_width / 2))
                               && ((ball_y <= bat_h / 2 + bat_y) && (ball_y >= -bat_h / 2 + bat_y))
    rightXRange x ball_y bat_y bat_h = (floor(x + ball_radius) ==  floor(bat1x - bat_width / 2))
                               && ((ball_y <= bat_h / 2 + bat_y) && (ball_y >= -bat_h / 2 + bat_y))

-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
chgBallVwall :: BS -> Boundary -> BS
chgBallVwall ballStat TopBound    = ballStat {vely = -vely ballStat}
chgBallVwall ballStat BottomBound = ballStat {vely = -vely ballStat}
chgBallVwall ballStat _           = ballStat

chgBat1wall :: Bat1 -> Bat1
chgBat1wall bat1Stat = bat1Stat { bat1 = if bat1 bat1Stat >= (boundary_height - bat1_len bat1Stat) / 2
                                         then (boundary_height - bat1_len bat1Stat) / 2
                                         else if bat1 bat1Stat <=  -(boundary_height - bat1_len bat1Stat) / 2
                                         then -(boundary_height - bat1_len bat1Stat) / 2
                                         else bat1 bat1Stat }

wallBounce :: PPG -> PPG
wallBounce game = game { ballStat = chgBallVwall (ballStat game) (outDirChk (ballStat game))
                       , bat1Stat = chgBat1wall (bat1Stat game)
                       , bat2 = y''' }
  where
    p2y = bat2 game
    y''' = if p2y >=  (boundary_height - (bat2_height game)) / 2
           then (boundary_height - (bat2_height game)) / 2
           else if p2y <=  -(boundary_height - (bat2_height game)) / 2
           then -(boundary_height - (bat2_height game)) / 2
           else p2y

outDirChk :: BS -> Boundary
outDirChk ballStat = if posx ballStat <= ball_radius - boundary_width /2       then LeftBound
                     else if posx ballStat >= boundary_width /2 - ball_radius  then RightBound
                     else if posy ballStat <= ball_radius - boundary_height /2 then TopBound
                     else if posy ballStat >= boundary_height /2 - ball_radius then BottomBound
                     else Center

-- | Judge Win/Lose
outofBound :: PPG -> PPG
outofBound game = case outDirChk (ballStat game) of
                    LeftBound  -> game {p1score = (p1score game) + 1, ballStat = initballState }
                    RightBound -> game {p2score = (p2score game) + 1, ballStat = initballState}
                    _          -> game

finishCheck :: PPG -> PPG
finishCheck game = if  (p2score game) == win_score || (p1score game) == win_score
                   then game {sceneState = End}
                   else game

-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> PPG -> PPG
update seconds = finishCheck . outofBound . batBounce . wallBounce . movement seconds