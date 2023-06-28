{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Football.Understanding.LineBreakingSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Football.Match (Match(..), AttackingDirection (..))
import Football.Types
import Football.Understanding.Space (pitchHorizontalZone)
import Football.Understanding.Space.Data
import Football.Understanding.Space.Data (HorizontalZone(CentreHZ))
import Control.Monad.Cont (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask, local), asks)
import Core (Cache(..))
import Linear (V3(V3), V2 (V2))
import Football.Understanding.LineBreaking (oppositionLines)

defaultPlayerSpeed :: PlayerSpeed
defaultPlayerSpeed =
  PlayerSpeed
    { playerSpeedAcceleration = 1.3
    , playerSpeedMax = 7.9
    }

data LineBreakingSpecContext = 
  LineBreakingSpecContext
    { lbPlayers :: [Player]
    , lbBall :: Ball
    }

newtype LineBreakingSpecM a = LineBreakingSpecM (ReaderT LineBreakingSpecContext IO a) deriving (Functor, Applicative, Monad, MonadIO)

runLineBreakingSpecM :: LineBreakingSpecContext -> LineBreakingSpecM a -> IO a
runLineBreakingSpecM pc (LineBreakingSpecM m) = runReaderT m pc

instance MonadReader LineBreakingSpecContext LineBreakingSpecM where
  ask = LineBreakingSpecM ask
  local f (LineBreakingSpecM x) = LineBreakingSpecM $ local f x

instance Match LineBreakingSpecM where
  attackingDirection Team1 = pure AttackingLeftToRight
  attackingDirection Team2 = pure AttackingRightToLeft
  gameBall = asks lbBall
  allPlayers = asks lbPlayers
  pitch = pure $ Pitch 105 68

instance Cache LineBreakingSpecM SpaceCache where
  cacheLookup _ = pure Nothing
  cacheInsert _ _ = pure ()


dmPlayer :: Player
dmPlayer = 
  Player 
    { playerPositionVector = V3 30 0 0
    , playerMotionVector = V3 0 0 0
    , playerNumber = 6
    , playerSpeed = defaultPlayerSpeed
    , playerIntention = DoNothing
    , playerTeam = Team2 
    }

leftCMPlayer :: Player
leftCMPlayer = 
  Player 
    { playerPositionVector = V3 25 8 0
    , playerMotionVector = V3 0 0 0
    , playerNumber = 10
    , playerSpeed = defaultPlayerSpeed
    , playerIntention = DoNothing
    , playerTeam = Team2 
    }

rightCMPlayer :: Player
rightCMPlayer = 
  Player 
    { playerPositionVector = V3 25 (-8) 0
    , playerMotionVector = V3 0 0 0
    , playerNumber = 8
    , playerSpeed = defaultPlayerSpeed
    , playerIntention = DoNothing
    , playerTeam = Team2 
    }

fwPlayer :: Player
fwPlayer = 
  Player 
    { playerPositionVector = V3 10 0 0
    , playerMotionVector = V3 0 0 0
    , playerNumber = 9
    , playerSpeed = defaultPlayerSpeed
    , playerIntention = DoNothing
    , playerTeam = Team2 
    }

ball :: Ball
ball = 
  Ball
    { ballPositionVector = V3 0.25 0 0
    , ballMotionVector = V3 0 0 0
    }

lineBreakingSpecTests :: TestTree
lineBreakingSpecTests = testGroup "LineBreakingSpec tests"
  [ testCase "Opposition Lines for a single player should cross the pitch (-y, +y) at that player's location" $ do
      let testContext =
            LineBreakingSpecContext
              { lbPlayers = [ dmPlayer ]
              , lbBall = ball
              }

      lines <- runLineBreakingSpecM testContext $ oppositionLines Team1

      lines @?= [(V2 30.0 39.0, V2 30.0 (-39.0))]
  , testCase "Opposition Lines for two players in a line toward the goal should cross the pitch (-y, +y) at those players' locations" $ do
      let testContext =
            LineBreakingSpecContext
              { lbPlayers = [ dmPlayer, fwPlayer ]
              , lbBall = ball
              }

      lines <- runLineBreakingSpecM testContext $ oppositionLines Team1

      lines @?= [(V2 10.0 (-39.0), V2 10.0 39.0), (V2 30.0 39.0, V2 30.0 (-39.0))]
  , testCase "Opposition Lines for two players in a line side by side should split the pitch in two in the y direction, meeting in the centre" $ do
      let testContext =
            LineBreakingSpecContext
              { lbPlayers = [ leftCMPlayer, rightCMPlayer ]
              , lbBall = ball
              }

      lines <- runLineBreakingSpecM testContext $ oppositionLines Team1

      lines @?= [(V2 25.0 (-39.0),V2 25.0 0.0),(V2 25.0 39.0,V2 25.0 0.0)]



  ]

