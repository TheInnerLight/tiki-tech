{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Football.Behaviours.PassSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Football.Match (Match(..), AttackingDirection (AttackingLeftToRight, AttackingRightToLeft))
import Football.Behaviours.Pass
import Linear (V3(V3), normalize)
import Control.Monad.Reader (ReaderT (runReaderT), MonadIO (liftIO), MonadReader (..), asks)
import Core
import Data.List (sortOn, find)
import qualified Data.Ord
import Football.Types
import Football.Understanding.Space.Data (SpaceCache)
import Data.Maybe (Maybe(Nothing), fromJust)

defaultPlayerSpeed :: PlayerSpeed
defaultPlayerSpeed =
  PlayerSpeed
    { playerSpeedAcceleration = 1.3
    , playerSpeedMax = 7.9
    }


data PassSpecContext = 
  PassSpecContext
    { psPlayers :: [PlayerState]
    , psBall :: Ball
    }

newtype PassSpecM a = PassSpecM (ReaderT PassSpecContext IO a) deriving (Functor, Applicative, Monad, MonadIO)

runPassSpecM :: PassSpecContext -> PassSpecM a -> IO a
runPassSpecM pc (PassSpecM m) = runReaderT m pc

instance MonadReader PassSpecContext PassSpecM where
  ask = PassSpecM ask
  local f (PassSpecM x) = PassSpecM $ local f x

instance Match PassSpecM where
  attackingDirection TeamId1 = pure AttackingLeftToRight
  attackingDirection TeamId2 = pure AttackingRightToLeft
  gameBall = asks psBall
  getPlayerState player = do
    players <- asks psPlayers
    pure $ fromJust $ find (\p -> player == playerStatePlayer p) players
  allPlayers = asks psPlayers
  pitch = pure $ Pitch 105 68

instance Log PassSpecM where
  logOutput stuff = liftIO $ print stuff

instance Cache PassSpecM SpaceCache where
  cacheLookup _ = pure Nothing
  cacheInsert _ _ = pure ()

playerWithBallAtOrigin :: PlayerState
playerWithBallAtOrigin = PlayerState
  { playerStatePlayer = Player 
      { playerNumber = 8
      , playerSpeed = defaultPlayerSpeed
      , playerTeamId = TeamId1
      }
  , playerStatePositionVector = V3 0 0 0
  , playerStateMotionVector = V3 0.0 0.0 0.0 
  , playerStateIntention = DoNothing
  }

easyPassingOptionTeammate :: PlayerState
easyPassingOptionTeammate = PlayerState
  { playerStatePlayer = Player 
      { playerNumber = 10
      , playerSpeed = defaultPlayerSpeed
      , playerTeamId = TeamId1
      }
  , playerStatePositionVector = V3 10 0 0
  , playerStateMotionVector = V3 0.0 0.0 0.0 
  , playerStateIntention = DoNothing
  }

farAwayOpposition1 :: PlayerState
farAwayOpposition1 = PlayerState
  { playerStatePlayer = Player 
      { playerNumber = 8
      , playerSpeed = defaultPlayerSpeed
      , playerTeamId = TeamId2
      }
  , playerStatePositionVector = V3 51 0 0
  , playerStateMotionVector = V3 0.0 0.0 0.0 
  , playerStateIntention = DoNothing
  }

farAwayOpposition2 :: PlayerState
farAwayOpposition2 = PlayerState
  { playerStatePlayer = Player 
      { playerNumber = 10
      , playerSpeed = defaultPlayerSpeed
      , playerTeamId = TeamId2
      }
  , playerStatePositionVector = V3 50 0 0
  , playerStateMotionVector = V3 0.0 0.0 0.0 
  , playerStateIntention = DoNothing
  }

farAwayTeammate :: PlayerState
farAwayTeammate = PlayerState
  { playerStatePlayer = Player 
      { playerNumber = 9
      , playerSpeed = defaultPlayerSpeed
      , playerTeamId = TeamId1
      }
  , playerStatePositionVector = V3 45 0 0
  , playerStateMotionVector = V3 0.0 0.0 0.0 
  , playerStateIntention = DoNothing
  }

equidistantTeammate :: V3 Double -> PlayerState
equidistantTeammate motion = PlayerState
  { playerStatePlayer = Player 
      { playerNumber = 9
      , playerSpeed = defaultPlayerSpeed
      , playerTeamId = TeamId1
      }
  , playerStatePositionVector = V3 30 0 0
  , playerStateMotionVector = motion
  , playerStateIntention = DoNothing
  }

equidistantOpponent :: V3 Double -> PlayerState
equidistantOpponent motion = PlayerState
  { playerStatePlayer = Player 
      { playerNumber = 9
      , playerSpeed = defaultPlayerSpeed
      , playerTeamId = TeamId2
      }
  , playerStatePositionVector = V3 30 1 0
  , playerStateMotionVector = motion
  , playerStateIntention = DoNothing
  }

highLineOpposingCentreBack1 :: PlayerState
highLineOpposingCentreBack1 = PlayerState
  { playerStatePlayer = Player 
      { playerNumber = 4
      , playerSpeed = defaultPlayerSpeed
      , playerTeamId = TeamId2
      }
  , playerStatePositionVector = V3 20 (-5) 0
  , playerStateMotionVector = V3 0.0 0.0 0.0 
  , playerStateIntention = DoNothing
  }

highLineOpposingCentreBack2 :: PlayerState
highLineOpposingCentreBack2 = PlayerState
  { playerStatePlayer = Player 
      { playerNumber = 5
      , playerSpeed = defaultPlayerSpeed
      , playerTeamId = TeamId2
      }
  , playerStatePositionVector = V3 20 5 0
  , playerStateMotionVector = V3 0.0 0.0 0.0 
  , playerStateIntention = DoNothing
  }

runningNearLineTeammate :: V3 Double -> PlayerState
runningNearLineTeammate motion = PlayerState
  { playerStatePlayer = Player 
      { playerNumber = 9
      , playerSpeed = defaultPlayerSpeed
      , playerTeamId = TeamId1
      }
  , playerStatePositionVector = V3 15 0 0
  , playerStateMotionVector = motion
  , playerStateIntention = DoNothing
  }

ball :: Ball
ball = 
  Ball
    { ballPositionVector = V3 0.25 0 0
    , ballMotionVector = V3 0 0 0
    }

passTests :: TestTree
passTests = testGroup "Pass Tests" [passToFeetTests, throughBallTests]

passToFeetTests :: TestTree
passToFeetTests = testGroup "Passing to feet tests"
  [ testCase "A stationary player will see a teammate as a passing option" $ do
      let testContext =
            PassSpecContext
              { psPlayers = [playerWithBallAtOrigin, easyPassingOptionTeammate, farAwayOpposition1, farAwayOpposition2]
              , psBall = ball
              }
      
      res <- runPassSpecM testContext $ do
        toFeetPassingOptions $ playerStatePlayer playerWithBallAtOrigin
      
      passTarget (head res) @?= PlayerTarget (playerStatePlayer easyPassingOptionTeammate)

  , testCase "A stationary player with two passing options will see the closer teammate further from opposition as the safest passing option" $ do
      let testContext =
            PassSpecContext
              { psPlayers = [playerWithBallAtOrigin, easyPassingOptionTeammate, farAwayTeammate, farAwayOpposition1, farAwayOpposition2]
              , psBall = ball
              }
      
      res <- runPassSpecM testContext $ do
        sortOn (Data.Ord.Down . passSafetyCoeff) <$> toFeetPassingOptions (playerStatePlayer playerWithBallAtOrigin)
     
      passTarget (head res) @?= PlayerTarget (playerStatePlayer easyPassingOptionTeammate)
      passTarget (last res) @?= PlayerTarget (playerStatePlayer farAwayTeammate)

  , testCase "A stationary player will see approximately 61% chance of pass completion when equidistant teammate and opponent" $ do
    -- Odds are not 50/50 due to b term in 1 / 1 + exp (-(az+b)) : Space evaluation in football games via field weighting based on tracking data
      let testContext =
            PassSpecContext
              { psPlayers = [playerWithBallAtOrigin, equidistantTeammate $ V3 0 0 0, equidistantOpponent $ V3 0 0 0, farAwayOpposition1]
              , psBall = ball
              }
      
      res <- runPassSpecM testContext $ do
        sortOn (Data.Ord.Down . passSafetyCoeff) <$> toFeetPassingOptions (playerStatePlayer playerWithBallAtOrigin)

      passTarget (head res) @?= PlayerTarget (playerStatePlayer $ equidistantTeammate $ V3 0 0 0)
      passSafetyCoeff (head res) `compare` 0.60 @?= GT
      passSafetyCoeff (head res) `compare` 0.65 @?= LT

  , testCase "A stationary player will see lower chance of pass completion with equidistant teammate and opponent running toward the ball" $ do
    let diff = normalize (playerStatePositionVector . equidistantOpponent $ V3 0 0 0 - playerStatePositionVector playerWithBallAtOrigin)
        testContext =
          PassSpecContext
            { psPlayers = [playerWithBallAtOrigin,  equidistantTeammate $ V3 0 0 0, equidistantOpponent (pure (-7.6) * diff), farAwayOpposition1]
            , psBall = ball
            }
    
    res <- runPassSpecM testContext $ do
      sortOn (Data.Ord.Down . passSafetyCoeff) <$> toFeetPassingOptions (playerStatePlayer playerWithBallAtOrigin)

    passTarget (head res) @?= PlayerTarget (playerStatePlayer $ equidistantTeammate $ V3 0 0 0)
    passSafetyCoeff (head res) `compare` 0.45 @?= LT

  , testCase "A stationary player will see higher chance of pass completion with equidistant teammate and opponent running away from the ball" $ do
    let diff = normalize (playerStatePositionVector . equidistantOpponent $ V3 0 0 0 - playerStatePositionVector playerWithBallAtOrigin)
        testContext =
          PassSpecContext
            { psPlayers = [playerWithBallAtOrigin, equidistantTeammate $ V3 0 0 0, equidistantOpponent (pure 7.6 * diff), farAwayOpposition1]
            , psBall = ball
            }

    res <- runPassSpecM testContext $ do
      sortOn (Data.Ord.Down . passSafetyCoeff) <$> toFeetPassingOptions (playerStatePlayer playerWithBallAtOrigin)

    passTarget (head res) @?= PlayerTarget (playerStatePlayer $ equidistantTeammate $ V3 0 0 0)
    passSafetyCoeff (head res) `compare` 0.81 @?= GT
  ]


throughBallTests :: TestTree
throughBallTests = testGroup "Through ball tests"
  [ testCase "There should be no through ball available if teammate is not moving" $ do
    let testContext =
          PassSpecContext
            { psPlayers = [playerWithBallAtOrigin, runningNearLineTeammate $ V3 0 0 0, highLineOpposingCentreBack1, highLineOpposingCentreBack2]
            , psBall = ball
            }
    
    res <- runPassSpecM testContext $ do
      sortOn (Data.Ord.Down . passSafetyCoeff) <$> throughBallPassingOptions (playerStatePlayer playerWithBallAtOrigin)

    res @?= []
    
  , testCase "There should be a through ball available to a teammate running behind the opponent line" $ do
    let testContext =
          PassSpecContext
            { psPlayers = [playerWithBallAtOrigin, runningNearLineTeammate $ V3 7.6 0 0, highLineOpposingCentreBack1, highLineOpposingCentreBack2]
            , psBall = ball
            }
    
    res <- runPassSpecM testContext $ do
      sortOn (Data.Ord.Down . passSafetyCoeff) <$> throughBallPassingOptions (playerStatePlayer playerWithBallAtOrigin)

    passTarget (head res) @?= AheadOfTarget (playerStatePlayer $ runningNearLineTeammate $ V3 7.6 0 0)

  , testCase "There should be no through ball available if teammate is running away from the opponent line" $ do
    let testContext =
          PassSpecContext
            { psPlayers = [playerWithBallAtOrigin, runningNearLineTeammate $ V3 (-7.6) 0 0, highLineOpposingCentreBack1, highLineOpposingCentreBack2]
            , psBall = ball
            }
    
    res <- runPassSpecM testContext $ do
      sortOn (Data.Ord.Down . passSafetyCoeff) <$> throughBallPassingOptions (playerStatePlayer $ playerWithBallAtOrigin)

    res @?= []

  ]
