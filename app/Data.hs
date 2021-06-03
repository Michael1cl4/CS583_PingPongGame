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

data PlayMod = WithUser
             | WithAI
  deriving (Eq, Show)

data SceneStat = Instruction PlayMod
               | Play PlayMod
               | End
  deriving (Eq, Show)

-- | Boundary for the movement of ball and bat
data Boundary = RightBound
              | LeftBound
              | TopBound
              | BottomBound
              | Center
  deriving (Eq, Show)

-- | Ball Status Data
data BS = BallStatue
  { posx :: Float  -- ^ Pong ball (x, y) Position.
  , posy :: Float
  , velx :: Float  -- ^ Pong ball (x, y) Velocity. 
  , vely :: Float
  , ballspeed :: Float
  }deriving Show

-- | Ping Pong Game State 
data PPG = Game
  { ballStat :: BS
  , bat1 :: Float                     -- ^ Left player bat height.
                                            -- Zero is the middle of the screen. 
  , bat2 :: Float                      -- ^ Right player bat height.
  , bat1state :: Int                 -- 0: stop, 1: move up, 2: move down
  , bat2state :: Int                 -- 0: stop, 1: move up, 2: move down
  , sceneState :: SceneStat
  , p1score :: Int
  , p2score :: Int
  , bat1_height :: Float
  , bat2_height :: Float
  } deriving Show

-- | The Initial State of the PPG
initialState :: PPG
initialState = Game
  { ballStat = initballState
  , bat1 = 40
  , bat2 = -80
  , bat1state = 0
  , bat2state = 0
  , sceneState = Instruction WithUser
  , p1score = 0
  , p2score = 0
  , bat1_height = 80
  , bat2_height = 80
  }

-- | The Initial State of the BS
initballState :: BS
initballState = BallStatue
  { posx = -10
  , posy = 30
  , velx = -30
  , vely = -40
  , ballspeed = 1.2
  }

-- | For Reading the function much easier 
type Radius = Float 
type Position = (Float, Float)