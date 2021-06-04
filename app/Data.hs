module Data where
-- | The size of our playground
boundary_width, boundary_height, boundary_offset :: Float
boundary_width  = 300
boundary_height = 300
boundary_offset = 100

-- | size of our window
window_width, window_height :: Float
window_width  = 400
window_height = 400

-- | size of the font
small_font_size, mid_font_size, large_font_size :: Float
small_font_size = 0.12
mid_font_size   = 0.2
large_font_size = 0.4

-- | a value that can slightly adjust the position of different instructions
instruction_adjust :: Float
instruction_adjust = 20

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
win_score = 2

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
data Bat = BatStatus
  { bat     :: Float  -- Position Y
  , bat_len :: Float  -- Length
  , motion  :: Motion -- Motion
  , score   :: Int
  }deriving Show

-- | Ping Pong Game State 
data PPG = Game
  { ballStat   :: BS          -- Ball Status
  , bat1Stat   :: Bat         -- Left player bat Status
  , bat2Stat   :: Bat
  , sceneState :: SceneStat -- Scene Status
  } deriving Show

-- | The Initial State of the PPG
initialState :: PPG
initialState = Game
  { ballStat = initballState
  , bat1Stat = initbat1State
  , bat2Stat = initbat2State
  , sceneState = Instruction WithUser
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
initbat1State :: Bat
initbat1State = BatStatus
  { bat     = 40   -- Position Y
  , bat_len = 80   -- Length
  , motion  = BStop -- Motion
  , score   = 0
  }

initbat2State :: Bat
initbat2State = BatStatus
  { bat     = -80   -- Position Y
  , bat_len = 80   -- Length
  , motion  = BStop -- Motion
  , score   = 0
  }

-- | For Reading the function much easier 
type Radius = Float 
type Position = (Float, Float)