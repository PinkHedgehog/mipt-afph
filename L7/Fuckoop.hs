{-# LANGUAGE TemplateHaskell, RankNTypes #-}

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import GHC.Base (undefined)


data Game = Game
          { _score :: Int
          , _units :: [Unit]
          , _boss  :: Unit
          }
          deriving (Show)

data Unit = Unit
          { _health   :: Int
          , _position :: Point
          } deriving (Show)

data Point = Point {_x :: Double, _y :: Double} deriving (Show)

-- score :: Lens' Game Int
-- score = lens _score (\game v -> game { _score = v })

-- units :: Lens' Game [Unit]
-- units = lens _units (\game v -> game { _units = v })

makeLenses ''Game
makeLenses ''Unit
makeLenses ''Point

inititalState :: Game
inititalState = Game
              { _score = 0
              , _units = [ Unit { _health   = 10
                                , _position =  Point 3.5 7.0
                                }
                         , Unit { _health   = 15
                                , _position =  Point 1.0 1.0
                                }
                         , Unit { _health   = 8
                                , _position =  Point 0.0 2.1
                                }

                         ]
              , _boss = Unit 100 (Point 0.0 0.0)
              }

strike :: StateT Game IO ()
strike = do
    lift $ do
        putStr "*shink*"
    bossHP -= 10

bossHP :: Lens' Game Int
bossHP = boss.health

partyHP :: Traversal' Game Int
partyHP = units.traversed.health


around center radius = filtered $ \unit ->
    (unit^.position.x - center^.x) ** 2.0
        + (unit^.position.y - center^.y) ** 2.0
        < radius ** 2.0

-- around = undefined        

fireBreath :: Point -> StateT Game IO ()
fireBreath target = do
    lift $ putStrLn "*Waaaagh!*"
    units.traversed.(around target 1.0).health -= 3

