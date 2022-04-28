
module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Interact
import System.Win32.Time
-- | Data describing the state of the pong game.
data PongGame = Game
    { ballLoc  :: (Float, Float) -- ^ Pong ball (x, y) location.
    , ballVel  :: (Float, Float) -- ^ Pong ball (x, y) velocity.
    , player1  :: Float          -- ^ Left player paddle height.
                                 -- Zero is the middle of the screen.
    , player2  :: Float          -- ^ Right player paddle height.
    , isPaused :: Bool           -- ^ Whether the game is paused
    , score1   :: Int            -- ^ First player's score
    , score2   :: Int            -- ^ Second player's score
    , lastPt   :: Int
    } deriving Show

-- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
    { ballLoc  = (10, 30)
    , ballVel  = (-25, -10)
    , player1  = 40
    , player2  = 40
    , isPaused = False
    , score1   = 0
    , score2   = 0
    , lastPt   = 0
    }


main :: IO ()
main = do
    x <- getSystemTime
    let y = read (show $ wSecond x) :: Float
    play window background fps (initialState' y) render handleKeys update
        where
            initialState' y = initialState { ballVel = (fromIntegral $ 25 * (-1) ^ (round y :: Int), 2 + y / 6) }

--
-- | Update the game by moving the ball.
-- Ignore the ViewPort argument.
update ::  Float -> PongGame -> PongGame
update seconds game
    | isPaused game = game
    | otherwise     = checkWin.checkScore.paddleBounce.wallBounce $ moveBall seconds game


-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame

-- For an 's' keypress, reset the ball to the center.
handleKeys (EventKey (Char 'r') _ _ _) game = game { ballLoc = (0, 0)
                                                   , lastPt  = 0
                                                   , score1  = 0
                                                   , score2  = 0
                                                   , ballVel = (5, -15)
                                                   }
--Pause
handleKeys (EventKey (Char 'p') Down _ _) game = game { isPaused = p' }
    where p' = not $ isPaused game

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game { player1 = p1' }
    where
        p1' | h >= 145  = player1 game
            | otherwise = player1 game + (fromIntegral (score1 game  * 2) :: Float) + 7
        h = player1 game + 43
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = game { player1 = p1' }
    where
        p1' | h <= -145 = player1 game
            | otherwise = player1 game - 7 - (fromIntegral (score1 game  * 2) :: Float)
        h = player1 game - 43
handleKeys (EventKey (Char 'w') Down _ _) game = game { player2 = p2' }
    where
        p2' | h >= 145  = player2 game
            | otherwise = player2 game + 7 + (fromIntegral (score2 game  * 2) :: Float)
        h = player2 game + 43
handleKeys (EventKey (Char 's') Down _ _) game = game { player2 = p2' }
    where
        p2' | h <= -145 = player2 game
            | otherwise = player2 game - 7 - (fromIntegral (score2 game * 2) :: Float)
        h = player2 game - 43
-- Do nothing for all other events.
handleKeys (EventKey (SpecialKey KeyEsc) Down _ _) _ = error "Escape"
handleKeys _ game = game


-- | Convert a game state into a picture.
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game = pictures [ showScore
                       , ball
                       , walls
                       , mkPaddle rose 120 $ player1 game
                       , mkPaddle orange (-120) $ player2 game
                       ]
    where
        showScore | win = Color white $ translate (-100) 160 $
                    scale 0.1 0.1 $ Text $
                        "Player #" ++ (show $ lastPt game) ++ " won!\n press R to reset"
                  | otherwise = Color white $ translate (-15) 160 $ scale 0.1 0.1 $
                      Text $ (show $ score2 game) ++ " : " ++ (show $ score1 game)
                where
                    win = (score1 game == 5) || (score2 game == 5)
            --Color white $ translate (-15) 160 $ scale 0.1 0.1 $
            --    Text $ (show $ score1 game) ++ " : " ++ (show $ score1 game)
        -- The pong ball
        ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
        ballColor = dark red
        --  The bottom and top walls.
        wall :: Float -> Picture
        wall offset =
            translate 0 offset $
                color wallColor $
                    rectangleSolid 270 10

        wallColor = greyN 0.5
        walls = pictures [wall 150, wall (-150)]

        mkPaddle :: Color -> Float -> Float -> Picture
        mkPaddle col x y = pictures [ translate x y $ color col $ rectangleSolid 26 86
                                    , translate x y $ color paddleColor $ rectangleSolid 20 80
                                    ]
        paddleColor = light $ light blue


width, height, offset :: Int
width  = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height + 50) (offset, offset)

-- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y') }
    where
        -- Old locations and velocities.
        (x, y) = ballLoc game
        (vx, vy) = ballVel game

        -- New locations.
        x' = x + vx * seconds * 3
        y' = y + vy * seconds * 3

background :: Color
background = black

fps :: Int
fps = 60


-- | Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = ( 1.0002 * vx', 1.0002 * vy) }
    where
        radius = 10
        (vx, vy) = ballVel game
        vx' = if paddleCollision game (ballLoc game) radius
             then -vx
             else  vx



-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (1.00001 * vx, 1.00001 * vy') }
    where
        -- Radius. Use the same thing as in `render`.
        radius = 10

        -- The old velocities.
        (vx, vy) = ballVel game

        vy' = if wallCollision (ballLoc game) radius
              then
                -- Update the velocity.
                  -vy
              else
                -- Do nothing. Return the old velocity.
                   vy

-- | When ball is gone, change score and place ball in the middle
checkScore :: PongGame -> PongGame
checkScore game | ballRight = game { ballLoc = (10, 30)
                                   , score2  = sc2
                                   , ballVel = (-x'/(fromIntegral sc2) * 1.1, -y'/ (fromIntegral sc2) * 1.1)
                                   , lastPt  = 1
                                   }
                | ballLeft = game { ballLoc = (-10, 30)
                                  , score1  = sc1
                                  , ballVel = (x'/(fromIntegral sc1) * 1.1, y'/ (fromIntegral sc1) * 1.1)
                                  , lastPt  = 2
                                  }
                | otherwise = game
                where
                    sc1 = score1 game + 1
                    sc2 = score2 game + 1
                    ballRight = (x >= 135) && wallCollision (x, y) radius ||
                        (x - radius) >= 133
                    ballLeft = (x <= -135) && wallCollision (x, y) radius ||
                        (x + radius) <= -133
                    (x, y) = ballLoc game
                    (x', y') = ballVel game
                    radius = 10

checkWin :: PongGame -> PongGame
checkWin game | win       = game { isPaused = True }
              | otherwise = game
              where
                  win = (score1 game == 5) || (score2 game == 5)

type Radius = Float
type Position = (Float, Float)

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
    where
        topCollision    = y - radius <= -fromIntegral height / 2
        bottomCollision = y + radius >=  fromIntegral height / 2

paddleCollision :: PongGame -> Position -> Radius -> Bool
paddleCollision game (x, y) radius = leftCollision || rightCollision
    where
        leftCollision  = ((x + radius) >=  107) && (pdl player1 y)
        rightCollision = ((x - radius) <= -107) && (pdl player2 y)
        pdl side y = (y >= side game - 43) && (y <= side game + 43)
