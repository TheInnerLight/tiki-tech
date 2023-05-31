{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Football.Behaviours.PassSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Football.Match (Match(..), AttackingDirection (AttackingLeftToRight, AttackingRightToLeft))
import Football.Behaviours.Pass
import Linear (V3(V3), normalize)
import Control.Monad.Reader (ReaderT (runReaderT), MonadIO (liftIO), MonadReader (..), asks)
import Core (Log (logOutput))
import Data.List (sortOn)
import qualified Data.Ord
import Football.Types
import Football.Pitch (Pitch(..))

defaultPlayerSpeed :: PlayerSpeed
defaultPlayerSpeed =
  PlayerSpeed
    { playerSpeedAcceleration = 1.3
    , playerSpeedMax = 7.9
    }


data PassSpecContext = 
  PassSpecContext
    { psPlayers :: [Player]
    , psBall :: Ball
    }

newtype PassSpecM a = PassSpecM (ReaderT PassSpecContext IO a) deriving (Functor, Applicative, Monad, MonadIO)

runPassSpecM :: PassSpecContext -> PassSpecM a -> IO a
runPassSpecM pc (PassSpecM m) = runReaderT m pc

instance MonadReader PassSpecContext PassSpecM where
  ask = PassSpecM ask
  local f (PassSpecM x) = PassSpecM $ local f x


instance Match PassSpecM where
  attackingDirection Team1 = pure AttackingLeftToRight
  attackingDirection Team2 = pure AttackingRightToLeft
  gameBall = asks psBall
  allPlayers = asks psPlayers
  pitch = pure $ Pitch 105 68

instance Log PassSpecM where
  logOutput stuff = liftIO $ print stuff

playerWithBallAtOrigin :: Player
playerWithBallAtOrigin = 
  Player 
    { playerPositionVector = V3 0 0 0
    , playerMotionVector = V3 0 0 0
    , playerDesiredLocation = V3 0 0 0
    , playerNumber = 8
    , playerSpeed = defaultPlayerSpeed
    , playerIntention = DoNothing
    , playerTeam = Team1  
    }

easyPassingOptionTeammate :: Player
easyPassingOptionTeammate = 
  Player 
    { playerPositionVector = V3 10 0 0
    , playerMotionVector = V3 0 0 0
    , playerDesiredLocation = V3 0 0 0
    , playerNumber = 10
    , playerSpeed = defaultPlayerSpeed
    , playerIntention = DoNothing
    , playerTeam = Team1  
    }

farAwayOpposition1 :: Player
farAwayOpposition1 = 
  Player 
    { playerPositionVector = V3 100 0 0
    , playerMotionVector = V3 0 0 0
    , playerDesiredLocation = V3 0 0 0
    , playerNumber = 8
    , playerSpeed = defaultPlayerSpeed
    , playerIntention = DoNothing
    , playerTeam = Team2
    }

farAwayOpposition2 :: Player
farAwayOpposition2 = 
  Player 
    { playerPositionVector = V3 110 0 0
    , playerMotionVector = V3 0 0 0
    , playerDesiredLocation = V3 0 0 0
    , playerNumber = 10
    , playerSpeed = defaultPlayerSpeed
    , playerIntention = DoNothing
    , playerTeam = Team2
    }

farAwayTeammate :: Player
farAwayTeammate = 
  Player 
    { playerPositionVector = V3 90 0 0
    , playerMotionVector = V3 0 0 0
    , playerDesiredLocation = V3 0 0 0
    , playerNumber = 9
    , playerSpeed = defaultPlayerSpeed
    , playerIntention = DoNothing
    , playerTeam = Team1  
    }

equidistantTeammate :: V3 Double -> Player
equidistantTeammate motion = 
  Player 
    { playerPositionVector = V3 30 0 0
    , playerMotionVector = motion
    , playerDesiredLocation = V3 0 0 0
    , playerNumber = 9
    , playerSpeed = defaultPlayerSpeed
    , playerIntention = DoNothing
    , playerTeam = Team1  
    }

equidistantOpponent :: V3 Double -> Player
equidistantOpponent motion = 
  Player 
    { playerPositionVector = V3 30 1 0
    , playerMotionVector = motion
    , playerDesiredLocation = V3 0 0 0
    , playerNumber = 9
    , playerSpeed = defaultPlayerSpeed
    , playerIntention = DoNothing
    , playerTeam = Team2
    }

highLineOpposingCentreBack1 :: Player
highLineOpposingCentreBack1 = 
  Player 
    { playerPositionVector = V3 20 (-5) 0
    , playerMotionVector = V3 0 0 0
    , playerDesiredLocation = V3 0 0 0
    , playerNumber = 4
    , playerSpeed = defaultPlayerSpeed
    , playerIntention = DoNothing
    , playerTeam = Team2
    }

highLineOpposingCentreBack2 :: Player
highLineOpposingCentreBack2 = 
  Player 
    { playerPositionVector = V3 20 (5) 0
    , playerMotionVector = V3 0 0 0
    , playerDesiredLocation = V3 0 0 0
    , playerNumber = 5
    , playerSpeed = defaultPlayerSpeed
    , playerIntention = DoNothing
    , playerTeam = Team2
    }

runningNearLineTeammate :: V3 Double -> Player
runningNearLineTeammate motion = 
  Player 
    { playerPositionVector = V3 15 0 0
    , playerMotionVector = motion
    , playerDesiredLocation = V3 0 0 0
    , playerNumber = 9
    , playerSpeed = defaultPlayerSpeed
    , playerIntention = DoNothing
    , playerTeam = Team1  
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
  [ testCase "One stationary player will see one teammate as a passing option" $ do
      let testContext =
            PassSpecContext
              { psPlayers = [playerWithBallAtOrigin, easyPassingOptionTeammate, farAwayOpposition1, farAwayOpposition2]
              , psBall = ball
              }
      
      res <- runPassSpecM testContext $ do
        toFeetPassingOptions playerWithBallAtOrigin
      
      passTarget (head res) @?= PlayerTarget easyPassingOptionTeammate

  , testCase "One stationary player will see closer opponent further from opposition as safest passing option" $ do
      let testContext =
            PassSpecContext
              { psPlayers = [playerWithBallAtOrigin, easyPassingOptionTeammate, farAwayTeammate, farAwayOpposition1, farAwayOpposition2]
              , psBall = ball
              }
      
      res <- runPassSpecM testContext $ do
        sortOn (Data.Ord.Down . passSafetyCoeff) <$> toFeetPassingOptions playerWithBallAtOrigin
      
      passTarget (head res) @?= PlayerTarget easyPassingOptionTeammate
      passTarget (last res) @?= PlayerTarget farAwayTeammate

  , testCase "One stationary player will see approximately 61% chance of pass completion when equidistant teammate and opponent" $ do
    -- Odds are not 50/50 due to b term in 1 / 1 + exp (-(az+b)) : Space evaluation in football games via field weighting based on tracking data
      let testContext =
            PassSpecContext
              { psPlayers = [playerWithBallAtOrigin, equidistantTeammate $ V3 0 0 0, equidistantOpponent $ V3 0 0 0, farAwayOpposition1]
              , psBall = ball
              }
      
      res <- runPassSpecM testContext $ do
        sortOn (Data.Ord.Down . passSafetyCoeff) <$> toFeetPassingOptions playerWithBallAtOrigin

      passTarget (head res) @?= PlayerTarget (equidistantTeammate $ V3 0 0 0)
      passSafetyCoeff (head res) `compare` 0.63 @?= GT
      passSafetyCoeff (head res) `compare` 0.65 @?= LT

  , testCase "One stationary player will see lower chance of pass completion with equidistant teammate and opponent running toward the ball" $ do
    let diff = normalize (playerPositionVector . equidistantOpponent $ V3 0 0 0 - playerPositionVector playerWithBallAtOrigin)
        testContext =
          PassSpecContext
            { psPlayers = [playerWithBallAtOrigin,  equidistantTeammate $ V3 0 0 0, equidistantOpponent (pure (-7.6) * diff), farAwayOpposition1]
            , psBall = ball
            }
    
    res <- runPassSpecM testContext $ do
      sortOn (Data.Ord.Down . passSafetyCoeff) <$> toFeetPassingOptions playerWithBallAtOrigin

    passTarget (head res) @?= PlayerTarget (equidistantTeammate $ V3 0 0 0)
    passSafetyCoeff (head res) `compare` 0.45 @?= LT

  , testCase "One stationary player will see higher chance of pass completion with equidistant teammate and opponent running away from the ball" $ do
    let diff = normalize (playerPositionVector . equidistantOpponent $ V3 0 0 0 - playerPositionVector playerWithBallAtOrigin)
        testContext =
          PassSpecContext
            { psPlayers = [playerWithBallAtOrigin, equidistantTeammate $ V3 0 0 0, equidistantOpponent (pure 7.6 * diff), farAwayOpposition1]
            , psBall = ball
            }

    res <- runPassSpecM testContext $ do
      sortOn (Data.Ord.Down . passSafetyCoeff) <$> toFeetPassingOptions playerWithBallAtOrigin

    passTarget (head res) @?= PlayerTarget (equidistantTeammate $ V3 0 0 0)
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
      sortOn (Data.Ord.Down . passSafetyCoeff) <$> throughBallPassingOptions playerWithBallAtOrigin

    res @?= []
    
  , testCase "There should be a through ball available to a teammate running behind the opponent line" $ do
    let testContext =
          PassSpecContext
            { psPlayers = [playerWithBallAtOrigin, runningNearLineTeammate $ V3 7.6 0 0, highLineOpposingCentreBack1, highLineOpposingCentreBack2]
            , psBall = ball
            }
    
    res <- runPassSpecM testContext $ do
      sortOn (Data.Ord.Down . passSafetyCoeff) <$> throughBallPassingOptions playerWithBallAtOrigin

    passTarget (head res) @?= AheadOfTarget (runningNearLineTeammate $ V3 7.6 0 0)

  , testCase "There should be no through ball available if teammate is running away from the opponent line" $ do
    let testContext =
          PassSpecContext
            { psPlayers = [playerWithBallAtOrigin, runningNearLineTeammate $ V3 (-7.6) 0 0, highLineOpposingCentreBack1, highLineOpposingCentreBack2]
            , psBall = ball
            }
    
    res <- runPassSpecM testContext $ do
      sortOn (Data.Ord.Down . passSafetyCoeff) <$> throughBallPassingOptions playerWithBallAtOrigin

    res @?= []

  ]
