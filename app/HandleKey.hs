module HandleKey where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
import Data

setBat1Motion :: Bat1 -> Motion -> Bat1
setBat1Motion batstat a = batstat {motion = a}

tuneBat1Len :: Bat1 -> Bat1
tuneBat1Len bat1Stat = if bat1_len bat1Stat < 120 then bat1Stat {bat1_len = bat1_len bat1Stat + 20}
                       else bat1Stat {bat1_len = 40}

-- | Respond to key events.
handleKeys :: Event -> PPG -> PPG

-- For an 'space' keypress, reset the ball.
handleKeys (EventKey (SpecialKey KeySpace) _ _ _) game = game { ballStat = initballState }

-- For an KeyUp keypress, move up the bat2.
handleKeys (EventKey (Char 'w') Down _ _) game = game {bat2state = 1}
-- For an KeyDown keypress, move down the bat2.
handleKeys (EventKey (Char 's') Down _ _) game = game {bat2state = 2}
-- For an KeyPageUp keypress, move up the bat1.
handleKeys (EventKey (Char 'o') Down _ _) game = game {bat1Stat = setBat1Motion (bat1Stat game) BUp}
-- For an KeyPageDown keypress, move down the bat1.
handleKeys (EventKey (Char 'l') Down _ _) game = game {bat1Stat = setBat1Motion (bat1Stat game) BDown}

-- For an “1” keypress, move up the bat1.
handleKeys (EventKey (Char '1') Down _ _) game = case sceneState game of
                                                 Instruction WithUser -> game {bat1Stat = tuneBat1Len (bat1Stat game)}
                                                 _                    -> game
-- For an “2” keypress, move down the bat1.
handleKeys (EventKey (Char '2') Down _ _) game = case sceneState game of
                                                 Instruction WithUser ->  case (bat2_height game) of
                                                                          40 -> game {bat2_height = 60}
                                                                          60 -> game {bat2_height = 80}
                                                                          80 -> game {bat2_height = 100}
                                                                          100 -> game {bat2_height = 120}
                                                                          120 -> game {bat2_height = 40}
                                                 _                    -> game

handleKeys (EventKey (Char 's') Up _ _) game = game {bat2state = 0}
handleKeys (EventKey (Char 'w') Up _ _) game = game {bat2state = 0}
handleKeys (EventKey (Char 'o') Up _ _) game = game {bat1Stat = setBat1Motion (bat1Stat game) BStop}
handleKeys (EventKey (Char 'l') Up _ _) game = game {bat1Stat = setBat1Motion (bat1Stat game) BStop}

handleKeys (EventKey (Char 'm') Down _ _) game = case sceneState game of
                                                 Instruction WithUser -> game {sceneState = Instruction WithAI}
                                                 Instruction WithAI   -> game {sceneState = Instruction WithUser}
                                                 _                 -> game
-- Out of the End Scene
handleKeys (EventKey (Char 'q') Down _ _) game = case sceneState game of
                                                 Instruction a -> game {sceneState = Play a}
                                                 _             -> initialState

-- Do nothing for all other events.
handleKeys _ game = game