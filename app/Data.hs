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

-- | Indicate Bat's motion
data Motion = BStop
            | BUp
            | BDown
  deriving (Eq, Show)

-- | Play Mode data define
data PlayMod = WithUser
             | WithAI
  deriving (Eq, Show)

-- | SceneStat data define
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
  { posx :: Float  -- Pong ball (x, y) Position.
  , posy :: Float
  , velx :: Float  -- Pong ball (x, y) Velocity.
  , vely :: Float
  , ballspeed :: Float
  }deriving Show

-- | Left player Bat Status
data Bat1 = Bat1Status
  { bat1     :: Float  -- Position Y
  , bat1_len :: Float  -- Length
  , motion   :: Motion -- Motion
  }deriving Show

-- | Ping Pong Game State 
data PPG = Game
  { ballStat :: BS          -- Ball Status
  , bat1Stat :: Bat1        -- Left player bat Status
  , bat2 :: Float           -- Right player bat height.
  , bat2_height :: Float
  , bat2state :: Int        -- 0: stop, 1: move up, 2: move down
  , sceneState :: SceneStat -- Scene Status
  , p1score :: Int
  , p2score :: Int
  } deriving Show

-- | The Initial State of the PPG
initialState :: PPG
initialState = Game
  { ballStat = initballState
  , bat1Stat = initbat1State
  , bat2 = -80
  , bat2state = 0
  , bat2_height = 80
  , sceneState = Instruction WithUser
  , p1score = 0
  , p2score = 0
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

-- | The Initial State of the Bat1
initbat1State :: Bat1
initbat1State = Bat1Status
  { bat1     = 40   -- Position Y
  , bat1_len = 80   -- Length
  , motion   = BStop -- Motion
  }

-- | For Reading the function much easier 
type Radius = Float 
type Position = (Float, Float)