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
window = InWindow "Ping Pong Game" (round(window_width), round(window_height)) (0, 0)

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
main = play window background_Color fps initialState render handleKeys update

-- | Convert a game state into a picture.
render :: PPG  -> Picture
render game = case (sceneState game) of
                Instruction WithAI -> pictures [ai , instruction2, instruction2_diff, bat_shape (bat_len (bat2Stat game)) (-window_height/4 + instruction_adjust), next, welcome, mod]
                Instruction WithUser -> pictures [instruction1, instruction1_diff, bat_shape (bat_len (bat1Stat game)) 0, instruction2, instruction2_diff, bat_shape (bat_len (bat2Stat game)) (-window_height/4 + instruction_adjust), next, welcome, mod]
                Play _ -> pictures [ball, walls, mkBat rose bat1x (bat (bat1Stat game)) (bat_len (bat1Stat game)), mkBat orange bat2x (bat (bat2Stat game)) (bat_len (bat2Stat game)), player1_score, colon, player2_score]
                End -> pictures [endTitle, endSubtitle, endEdit1, endEdit2, endEdit3]
  where
    -- Instruction Scene
    welcome = translate (-window_width/2 + instruction_adjust) (window_height/4) (scale mid_font_size mid_font_size (text "Welcome to PingPong Game"))
    instruction1 = translate (-window_width/2 + instruction_adjust) (window_height/8) (scale small_font_size small_font_size (text "Player1 use O/L to control the right bat"))
    instruction1_diff = translate (-window_width/2 + instruction_adjust) (window_height/8 - instruction_adjust) (scale small_font_size small_font_size (text "Player1 use '1' to tune the bat length"))
    ai = translate (-window_width/4) (window_height/8) (scale small_font_size small_font_size (text "Player1 is controlled by AI"))
    bat_shape bat_len a = translate (0) (a) (color bat_Color (rectangleSolid bat_len (10)))
    instruction2 = translate (-window_width/2 + instruction_adjust) (-window_height/8 + instruction_adjust) (scale small_font_size small_font_size (text "Player2 use W/S to control the left bat"))
    instruction2_diff = translate (-window_width/2 + instruction_adjust) (-window_height/8) (scale small_font_size small_font_size (text "Player2 use '2' to tune the bat length"))
    next  = translate (-window_width/4) (-window_height/2.5 + instruction_adjust) (scale mid_font_size mid_font_size (text "Press Q to play"))
    mod  = translate (-window_width/2 + instruction_adjust) (-window_height/2 + instruction_adjust) (scale mid_font_size mid_font_size (text "Press M to change mode"))
    -- End Scene
    endTitle    = if (score (bat1Stat game) == win_score)
                  then translate (-window_width/2.5 + instruction_adjust) (window_height/4) (scale large_font_size large_font_size (text ("Player"++ show(1) ++" Win!!!")))
                  else translate (-window_width/2.5 + instruction_adjust) (window_height/4) (scale large_font_size large_font_size (text ("Player"++ show(2) ++" Win!!!")))
    endSubtitle = translate (-window_width/4 + instruction_adjust) (window_height/8)    (scale small_font_size small_font_size (text "[Game Developers]"))
    endEdit1  = translate (-window_width/4 - instruction_adjust * 2) 0 (scale small_font_size small_font_size (text "Yinchao Zhu zhuyin@oregonstate.edu"))
    endEdit2  = translate (-window_width/4 - instruction_adjust * 2) (0 - instruction_adjust * 2) (scale small_font_size small_font_size (text "Haoyuan Qiu qiuha@oregonstate.edu"))
    endEdit3  = translate (-window_width/4 - instruction_adjust * 2) (0 - instruction_adjust * 4) (scale small_font_size small_font_size (text "Shukan Nieh niehsh@oregonstate.edu"))
    -- Play Scene
    -- the current score
    player1_score = translate (window_width/8) (window_height/2.5) (scale mid_font_size mid_font_size (text (show (score (bat1Stat game)))))
    colon = translate 0 (window_height/2.5) (scale mid_font_size mid_font_size (text (":")))
    player2_score = translate (-window_width/8) (window_height/2.5) (scale mid_font_size mid_font_size (text (show (score (bat2Stat game)))))
    --  The pong ball.
    ball = uncurry translate (posx (ballStat game), posy (ballStat game)) (color ball_Color (circleSolid ball_radius))

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall boundary_offset =
      translate 0 boundary_offset (color wall_Color (rectangleSolid wall_width wall_height))

    walls = pictures [wall (boundary_height / 2), wall (boundary_height / 2 * (-1))]

    --  Make a bat of a given border and vertical offset.
    mkBat :: Color -> Float -> Float -> Float -> Picture
    mkBat col x y h = pictures [translate x y (color col (rectangleSolid bat_width h))]

-- | Update the ball position using its current velocity.
moveball :: BS -> Float -> BS
moveball ballStat seconds = ballStat { posx = posx ballStat + velx ballStat * seconds
                                     , posy = posy ballStat + vely ballStat * seconds}

-- | Update the bat position using its motion and the play mode.
movebat :: Bat -> BS -> Float -> PlayMod -> Bat
movebat batStat ballStat a WithUser = batStat { bat = case motion batStat of
                                                          BStop -> bat batStat
                                                          BUp   -> bat batStat + a
                                                          BDown -> bat batStat - a }
movebat batStat ballStat a WithAI   = batStat { bat = if posy ballStat > bat batStat
                                                          then bat batStat + a
                                                          else bat batStat - a}

-- | Update the bat position by the play mode.
movement :: Float -> PPG -> PPG
movement seconds game = case sceneState game of
                        Play WithUser -> game { ballStat = moveball (ballStat game) seconds
                                              , bat1Stat = movebat (bat1Stat game) (ballStat game) 5 WithUser
                                              , bat2Stat = movebat (bat2Stat game) (ballStat game) 5 WithUser}
                        Play WithAI   -> game { ballStat = moveball (ballStat game) seconds
                                              , bat1Stat = movebat (bat1Stat game) (ballStat game) x1 WithAI
                                              , bat2Stat = movebat (bat2Stat game) (ballStat game) 5 WithUser}
                        _             -> game
  where
    -- New Position of bat
    x1 = unsafePerformIO (getStdRandom (randomR (0.5, 1.5)))

-- | Detect a collision with a bat. Upon batCollision,
--   change the velocity of the ball to bounce it off the bat.
chgBallVbat :: BS -> Bool -> BS
chgBallVbat ballStat True  = ballStat { velx = -velx ballStat * ballspeed ballStat
                                      , vely = vely ballStat  * ballspeed ballStat}
chgBallVbat ballStat False = ballStat {velx = -velx ballStat}

batBounce :: PPG -> PPG
batBounce game = case batCollision game of
                 True  -> game{ballStat = chgBallVbat (ballStat game) (abs(velx (ballStat game)) * ballspeed (ballStat game) < 150 && abs(vely (ballStat game)) * ballspeed (ballStat game) < 200)
                              }
                 False -> game

batCollision :: PPG -> Bool
batCollision game = (leftXRange (posx (ballStat game)) (posy (ballStat game)) (bat (bat2Stat game)) (bat_len (bat2Stat game))
                 || rightXRange (posx (ballStat game)) (posy (ballStat game)) (bat (bat1Stat game)) (bat_len (bat1Stat game)))
  where
    leftXRange x ball_y bat_y bat_h = ( floor(x - ball_radius) == floor(bat2x + bat_width / 2))
                               && ((ball_y <= bat_h / 2 + bat_y) && (ball_y >= -bat_h / 2 + bat_y))
    rightXRange x ball_y bat_y bat_h = (floor(x + ball_radius) ==  floor(bat1x - bat_width / 2))
                               && ((ball_y <= bat_h / 2 + bat_y) && (ball_y >= -bat_h / 2 + bat_y))

-- | Detect a collision with one of the side walls. Upon outDirChk's boundary result,
--   update the velocity of the ball to bounce it off the wall.
chgBallVwall :: BS -> Boundary -> BS
chgBallVwall ballStat TopBound    = ballStat {vely = -vely ballStat}
chgBallVwall ballStat BottomBound = ballStat {vely = -vely ballStat}
chgBallVwall ballStat _           = ballStat

chgBatwall :: Bat -> Bat
chgBatwall batStat = batStat { bat = if bat batStat >= (boundary_height - bat_len batStat) / 2
                                         then (boundary_height - bat_len batStat) / 2
                                         else if bat batStat <=  -(boundary_height - bat_len batStat) / 2
                                         then -(boundary_height - bat_len batStat) / 2
                                         else bat batStat }

wallBounce :: PPG -> PPG
wallBounce game = game { ballStat = chgBallVwall (ballStat game) (outDirChk (ballStat game))
                       , bat1Stat = chgBatwall (bat1Stat game)
                       , bat2Stat = chgBatwall (bat2Stat game) }

outDirChk :: BS -> Boundary
outDirChk ballStat = if posx ballStat <= ball_radius - boundary_width /2       then LeftBound
                     else if posx ballStat >= boundary_width /2 - ball_radius  then RightBound
                     else if posy ballStat <= ball_radius - boundary_height /2 then TopBound
                     else if posy ballStat >= boundary_height /2 - ball_radius then BottomBound
                     else Center

-- | Judge Win/Lose for each matchup
--   Detect a collision with one of the side walls. Upon outDirChk's boundary result,
countscore :: Bat -> Bat
countscore batStat = batStat { score = score batStat  + 1 }

outofBound :: PPG -> PPG
outofBound game = case outDirChk (ballStat game) of
                    LeftBound  -> game {bat1Stat = countscore(bat1Stat game), ballStat = initballState }
                    RightBound -> game {bat2Stat = countscore(bat2Stat game), ballStat = initballState}
                    _          -> game

-- | Used to check who win the whole game by win_score
finishCheck :: PPG -> PPG
finishCheck game = if  score (bat1Stat game) == win_score || score (bat2Stat game) == win_score
                   then game {sceneState = End}
                   else game

-- | Update the game by applying the rule, and update the status of both ball and bats.
update :: Float -> PPG -> PPG
update seconds = finishCheck . outofBound . batBounce . wallBounce . movement seconds