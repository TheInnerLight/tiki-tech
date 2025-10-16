{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Football.Understanding.LineBreakingSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Football.Match (Match(..), AttackingDirection (..))
import Football.Types
import Football.Understanding.Space (pitchHorizontalZone)
import Football.Understanding.Space.Data

import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask, local), asks)
import Core (Cache(..))
import Linear (V3(V3), V2 (V2))
import Football.Understanding.LineBreaking (oppositionLines)
import Data.Maybe (fromJust)
import Data.Foldable (find)
import Control.Monad.IO.Class (MonadIO)

defaultPlayerSpeed :: PlayerSpeed
defaultPlayerSpeed =
  PlayerSpeed
    { playerSpeedAcceleration = 1.3
    , playerSpeedMax = 7.9
    }

data LineBreakingSpecContext = 
  LineBreakingSpecContext
    { lbPlayers :: [PlayerState]
    , lbBall :: Ball
    }

newtype LineBreakingSpecM a = LineBreakingSpecM (ReaderT LineBreakingSpecContext IO a) deriving (Functor, Applicative, Monad, MonadIO)

runLineBreakingSpecM :: LineBreakingSpecContext -> LineBreakingSpecM a -> IO a
runLineBreakingSpecM pc (LineBreakingSpecM m) = runReaderT m pc

instance MonadReader LineBreakingSpecContext LineBreakingSpecM where
  ask = LineBreakingSpecM ask
  local f (LineBreakingSpecM x) = LineBreakingSpecM $ local f x

instance Match LineBreakingSpecM where
  attackingDirection TeamId1 = pure AttackingLeftToRight
  attackingDirection TeamId2 = pure AttackingRightToLeft
  gameBall = asks lbBall
  getPlayerState player = do
    players <- asks lbPlayers
    pure $ fromJust $ find (\p -> player == playerStatePlayer p) players
  allPlayers = asks lbPlayers
  pitch = pure $ Pitch 105 68

instance Cache LineBreakingSpecM SpaceCache where
  cacheLookup _ = pure Nothing
  cacheInsert _ _ = pure ()


dmPlayer :: PlayerState
dmPlayer = PlayerState
  { playerStatePlayer = Player 
      { playerNumber = 6
      , playerSpeed = defaultPlayerSpeed
      , playerTeamId = TeamId2
      }
  , playerStatePositionVector = V3 30 0 0
  , playerStateMotionVector = V3 0.0 0.0 0.0 
  , playerStateIntention = DoNothing
  }

leftCMPlayer :: PlayerState
leftCMPlayer = PlayerState
  { playerStatePlayer = Player 
      { playerNumber = 10
      , playerSpeed = defaultPlayerSpeed
      , playerTeamId = TeamId2
      }
  , playerStatePositionVector = V3 25 8 0
  , playerStateMotionVector = V3 0.0 0.0 0.0 
  , playerStateIntention = DoNothing
  }

rightCMPlayer :: PlayerState
rightCMPlayer = PlayerState
  { playerStatePlayer = Player 
      { playerNumber = 8
      , playerSpeed = defaultPlayerSpeed
      , playerTeamId = TeamId2
      }
  , playerStatePositionVector = V3 25 (-8) 0
  , playerStateMotionVector = V3 0.0 0.0 0.0 
  , playerStateIntention = DoNothing
  }

fwPlayer :: PlayerState
fwPlayer = PlayerState
  { playerStatePlayer = Player 
      { playerNumber = 9
      , playerSpeed = defaultPlayerSpeed
      , playerTeamId = TeamId2
      }
  , playerStatePositionVector = V3 10 0 0
  , playerStateMotionVector = V3 0.0 0.0 0.0 
  , playerStateIntention = DoNothing
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

      lines <- runLineBreakingSpecM testContext $ oppositionLines TeamId1

      lines @?= [(V2 30.0 39.0, V2 30.0 (-39.0))]
  , testCase "Opposition Lines for two players in a line toward the goal should cross the pitch (-y, +y) at those players' locations" $ do
      let testContext =
            LineBreakingSpecContext
              { lbPlayers = [ dmPlayer, fwPlayer ]
              , lbBall = ball
              }

      lines <- runLineBreakingSpecM testContext $ oppositionLines TeamId1

      lines @?= [(V2 10.0 (-39.0), V2 10.0 39.0), (V2 30.0 39.0, V2 30.0 (-39.0))]
  , testCase "Opposition Lines for two players in a line side by side should split the pitch in two in the y direction, meeting in the centre" $ do
      let testContext =
            LineBreakingSpecContext
              { lbPlayers = [ leftCMPlayer, rightCMPlayer ]
              , lbBall = ball
              }

      lines <- runLineBreakingSpecM testContext $ oppositionLines TeamId1

      lines @?= [(V2 25.0 (-39.0),V2 25.0 0.0),(V2 25.0 39.0,V2 25.0 0.0)]



  ]

