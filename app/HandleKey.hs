module HandleKey where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
import Data

setBatMotion :: Bat -> Motion -> Bat
setBatMotion batstat a = batstat {motion = a}

tuneBatLen :: Bat -> Bat
tuneBatLen batstat = if bat_len batstat < 120 then batstat {bat_len = bat_len batstat + 20}
                       else batstat {bat_len = 40}

-- | Respond to key events.
handleKeys :: Event -> PPG -> PPG

-- For an 'space' keypress, reset the ball.
handleKeys (EventKey (SpecialKey KeySpace) _ _ _) game = game { ballStat = initballState }

-- For an KeyUp keypress, move up the bat2.
handleKeys (EventKey (Char 'w') Down _ _) game = game {bat2Stat = setBatMotion (bat2Stat game) BUp}
-- For an KeyDown keypress, move down the bat2.
handleKeys (EventKey (Char 's') Down _ _) game = game {bat2Stat = setBatMotion (bat2Stat game) BDown}
-- For an KeyPageUp keypress, move up the bat1.
handleKeys (EventKey (Char 'o') Down _ _) game = game {bat1Stat = setBatMotion (bat1Stat game) BUp}
-- For an KeyPageDown keypress, move down the bat1.
handleKeys (EventKey (Char 'l') Down _ _) game = game {bat1Stat = setBatMotion (bat1Stat game) BDown}

-- For an “1” keypress, move up the bat1.
handleKeys (EventKey (Char '1') Down _ _) game = case sceneState game of
                                                 Instruction WithUser -> game {bat1Stat = tuneBatLen (bat1Stat game)}
                                                 _                    -> game
-- For an “2” keypress, move down the bat1.
handleKeys (EventKey (Char '2') Down _ _) game = case sceneState game of
                                                 Instruction WithUser -> game {bat2Stat = tuneBatLen (bat2Stat game)}
                                                 _                    -> game

handleKeys (EventKey (Char 's') Up _ _) game = game {bat2Stat = setBatMotion (bat2Stat game) BStop}
handleKeys (EventKey (Char 'w') Up _ _) game = game {bat2Stat = setBatMotion (bat2Stat game) BStop}
handleKeys (EventKey (Char 'o') Up _ _) game = game {bat1Stat = setBatMotion (bat1Stat game) BStop}
handleKeys (EventKey (Char 'l') Up _ _) game = game {bat1Stat = setBatMotion (bat1Stat game) BStop}

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