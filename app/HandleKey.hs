module HandleKey where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
import Data
-- | Respond to key events.
handleKeys :: Event -> PPG -> PPG

-- For an 'space' keypress, reset the ball.
handleKeys (EventKey (SpecialKey KeySpace) _ _ _) game = game { ballStat = initballState }

-- For an KeyUp keypress, move up the bat1.
handleKeys (EventKey (Char 'w') Down _ _) game = game {bat2state = 1}
-- For an KeyDown keypress, move down the bat1.
handleKeys (EventKey (Char 's') Down _ _) game = game {bat2state = 2}
-- For an KeyPageUp keypress, move up the bat2.
handleKeys (EventKey (Char 'o') Down _ _) game = game {bat1state = 1}
-- For an KeyPageDown keypress, move down the bat2.
handleKeys (EventKey (Char 'l') Down _ _) game = game {bat1state = 2}

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
handleKeys (EventKey (Char 'o') Up _ _) game = if ((ai_mod game) == 0) then game {bat1state = 0}
                                                           else game
handleKeys (EventKey (Char 'l') Up _ _) game = if ((ai_mod game) == 0) then game {bat1state = 0}
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