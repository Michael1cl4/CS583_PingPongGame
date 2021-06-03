module Data where
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

ball_radius :: Float
ball_radius = 10

data Boundary = Right
              | Left
              | Top
              | Bottom
              | Center
  deriving (Eq, Show)

data BS = BallStatue
  { posx :: Float  -- ^ Pong ball (x, y) Position.
  , posy :: Float
  , velx :: Float  -- ^ Pong ball (x, y) Velocity. 
  , vely :: Float
  , ballspeed :: Float
  }deriving Show

initballState :: BS
initballState = BallStatue
  { posx = -10
  , posy = 30
  , velx = -30
  , vely = -40
  , ballspeed = 1.2
  }


-- | Ping Pong Game State 
data PPG = Game
  { ballStat :: BS
  , bat1 :: Float                     -- ^ Left player bat height.
                                            -- Zero is the middle of the screen. 
  , bat2 :: Float                      -- ^ Right player bat height.
  , bat1state :: Int                 -- 0: stop, 1: move up, 2: move down
  , bat2state :: Int                 -- 0: stop, 1: move up, 2: move down
  , sceneState :: Int                -- 0: Instruction, 1: Play, 2: End
  , p1score :: Int
  , p2score :: Int
  , bat1_height :: Float
  , bat2_height :: Float
  , ai_mod :: Int
  } deriving Show

-- | The Initial State of the PPG
initialState :: PPG
initialState = Game
  { ballStat = initballState
  , bat1 = 40
  , bat2 = -80
  , bat1state = 0
  , bat2state = 0
  , sceneState = 0
  , p1score = 0
  , p2score = 0
  , bat1_height = 80
  , bat2_height = 80
  , ai_mod = 0
  }

-- | For Reading the function much easier 
type Radius = Float 
type Position = (Float, Float)