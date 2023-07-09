{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Football.Understanding.ShapeSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Football.Match (Match(..), AttackingDirection (AttackingLeftToRight))
import Football.Types
import Football.Understanding.Space (pitchHorizontalZone)
import Football.Understanding.Space.Data
import Football.Understanding.Space.Data (HorizontalZone(CentreHZ))
import Football.Types
import Football.Understanding.Shape (outOfPossessionFormationRelativeTo)
import Linear (V2(V2), V3(V3))
import Core 
import Control.Monad.State (StateT (runStateT), MonadIO (liftIO), MonadState (get))

data ShapeSpecContext = ShapeSpecContext
  { shapeSpecContextPressingPlayer :: Maybe Player
  }

newtype TestMLR a = TestM {unTestMLR :: StateT ShapeSpecContext IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance Match TestMLR where
  attackingDirection _ = pure AttackingLeftToRight
  getPlayerState p = do
    ssc <- TestM get
    case shapeSpecContextPressingPlayer ssc of
      Just p' | p == p' -> pure $ PlayerState { playerStatePlayer = p, playerStateIntention = WinBallIntention (V2 0 0) (GameTime FirstHalf 0), playerStateMotionVector = V3 0 0 0, playerStatePositionVector = V3 0 0 0}
      _                 -> pure $ PlayerState { playerStatePlayer = p, playerStateIntention = DoNothing, playerStateMotionVector = V3 0 0 0, playerStatePositionVector = V3 0 0 0}
    

instance Log TestMLR where
  logOutput a = liftIO $ print a

runTestMLR  :: ShapeSpecContext -> TestMLR a -> IO a
runTestMLR ss mlr = do
  (a, _) <- flip runStateT ss $ unTestMLR mlr
  pure a

playerDefaultAcceleration :: Double
playerDefaultAcceleration = 1.3

playerDefaultMaxSpeed :: Double
playerDefaultMaxSpeed = 7.8

playerLFB :: Player
playerLFB = Player
  { playerNumber = 3
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
  , playerTeamId = TeamId2
  }

playerLCB :: Player
playerLCB = Player
  { playerNumber = 5
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
  , playerTeamId = TeamId2
  }

playerLWB :: Player
playerLWB = Player
  { playerNumber = 3
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
  , playerTeamId = TeamId2
  }

playerRWB :: Player
playerRWB = Player
  { playerNumber = 2
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
  , playerTeamId = TeamId2
  }

playerCCB :: Player
playerCCB = Player
  { playerNumber = 14
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
  , playerTeamId = TeamId2
  }

playerRCB :: Player
playerRCB = Player
  { playerNumber = 4
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
  , playerTeamId = TeamId2
  }

playerRFB :: Player
playerRFB = Player
  { playerNumber = 2
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
  , playerTeamId = TeamId2
  }

playerDM :: Player
playerDM = Player
  { playerNumber = 6
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
  , playerTeamId = TeamId2
  }

playerLCM :: Player
playerLCM = Player
  { playerNumber = 10
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
  , playerTeamId = TeamId2
  }

playerRCM :: Player
playerRCM = Player
  { playerNumber = 8
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
  , playerTeamId = TeamId2
  }

playerCF :: Player
playerCF = Player
  { playerNumber = 9
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
  , playerTeamId = TeamId2
  }

playerLCF :: Player
playerLCF = Player
  { playerNumber = 12
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
  , playerTeamId = TeamId2
  }

playerRCF :: Player
playerRCF = Player
  { playerNumber = 9
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
  , playerTeamId = TeamId2
  }

playerLW :: Player
playerLW = Player
  { playerNumber = 11
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
  , playerTeamId = TeamId2
  }

playerRW :: Player
playerRW = Player
  { playerNumber = 7
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
  , playerTeamId = TeamId2
  }


fourThreeThreeTeam :: Team
fourThreeThreeTeam = Team
  { teamName = "Liverbird"
  , teamFormation = Formation
    { formationLine1 = FourLine playerLFB playerLCB playerRCB playerRFB
    , formationLine2 = EmptyLine
    , formationLine3 = ThreeLine playerLCM playerDM playerRCM
    , formationLine4 = EmptyLine
    , formationLine5 = ThreeLine playerLW playerCF playerRW
    }
  }

threeFiveTwoTeam :: Team
threeFiveTwoTeam = Team
  { teamName = "Intra Milan"
  , teamFormation = Formation
    { formationLine1 = ThreeLine playerLCB playerCCB playerRCB 
    , formationLine2 = EmptyLine
    , formationLine3 = FiveLine playerLWB playerLCM playerDM playerRCM playerRWB
    , formationLine4 = EmptyLine
    , formationLine5 = TwoLine playerLCF playerRCF
    }
  }


shapeSpecTests :: TestTree
shapeSpecTests = testGroup "ShapeSpec tests"
  [ testCase "Back four are properly positioned" $ do

    let ssc = 
          ShapeSpecContext 
            { shapeSpecContextPressingPlayer = Nothing
            }
            
    (pLFB, pLCB, pRCB, pRFB) <- runTestMLR ssc $ do
      pLFB <- outOfPossessionFormationRelativeTo (teamFormation fourThreeThreeTeam) 1 0 playerLFB (V2 0 0)
      pLCB <- outOfPossessionFormationRelativeTo (teamFormation fourThreeThreeTeam) 1 0 playerLCB (V2 0 0)
      pRCB <- outOfPossessionFormationRelativeTo (teamFormation fourThreeThreeTeam) 1 0 playerRCB (V2 0 0)
      pRFB <- outOfPossessionFormationRelativeTo (teamFormation fourThreeThreeTeam) 1 0 playerRFB (V2 0 0)
      pure (pLFB, pLCB, pRCB, pRFB)

    pLFB @?= V2 (-15.0) (-9.0)
    pLCB @?= V2 (-15.0) (-3.0)
    pRCB @?= V2 (-15.0) (3.0)
    pRFB @?= V2 (-15.0) (9.0)

  , testCase "Back three are properly positioned" $ do
    let ssc = 
          ShapeSpecContext 
            { shapeSpecContextPressingPlayer = Nothing
            }

    (pLCB, pCCB, pRCB) <- runTestMLR ssc $ do
      
      pLCB <- outOfPossessionFormationRelativeTo (teamFormation threeFiveTwoTeam) 1 0 playerLCB (V2 0 0)
      pCCB <- outOfPossessionFormationRelativeTo (teamFormation threeFiveTwoTeam) 1 0 playerCCB (V2 0 0)
      pRCB <- outOfPossessionFormationRelativeTo (teamFormation threeFiveTwoTeam) 1 0 playerRCB (V2 0 0)
      
      pure (pLCB, pCCB, pRCB)

    
    pLCB @?= V2 (-15.0) (-6.0)
    pCCB @?= V2 (-15.0) (-0.0)
    pRCB @?= V2 (-15.0) (6.0)

  , testCase "Midfield five are properly positioned" $ do

    let ssc = 
          ShapeSpecContext 
            { shapeSpecContextPressingPlayer = Nothing
            }
            
    (pLWB, pLCM, pCDM, pRCM, pRWB) <- runTestMLR ssc $ do
      pLW <- outOfPossessionFormationRelativeTo (teamFormation threeFiveTwoTeam) 1 0 playerLWB (V2 0 0)
      pLCM <- outOfPossessionFormationRelativeTo (teamFormation threeFiveTwoTeam) 1 0 playerLCM (V2 0 0)
      pCDM <- outOfPossessionFormationRelativeTo (teamFormation threeFiveTwoTeam) 1 0 playerDM (V2 0 0)
      pRCM <- outOfPossessionFormationRelativeTo (teamFormation threeFiveTwoTeam) 1 0 playerRCM (V2 0 0)
      pRW <- outOfPossessionFormationRelativeTo (teamFormation threeFiveTwoTeam) 1 0 playerRWB (V2 0 0)
      pure (pLW, pLCM, pCDM, pRCM, pRW)

    pLWB @?= V2 (-5.0) (-12.0)
    pLCM @?= V2 (-5.0) (-6.0)
    pCDM @?= V2 (-5.0) (0.0)
    pRCM @?= V2 (-5.0) (6.0)
    pRWB @?= V2 (-5.0) (12.0)

  , testCase "Midfield three are properly positioned" $ do
    let ssc = 
          ShapeSpecContext 
            { shapeSpecContextPressingPlayer = Nothing
            }

    (pLCM, pCDM, pRCM) <- runTestMLR ssc $ do
      
      pLCM <- outOfPossessionFormationRelativeTo (teamFormation fourThreeThreeTeam) 1 0 playerLCM (V2 0 0)
      pCDM <- outOfPossessionFormationRelativeTo (teamFormation fourThreeThreeTeam) 1 0 playerDM (V2 0 0)
      pRCM <- outOfPossessionFormationRelativeTo (teamFormation fourThreeThreeTeam) 1 0 playerRCM (V2 0 0)
      
      pure (pLCM, pCDM, pRCM)

    
    pLCM @?= V2 (-5.0) (-6.0)
    pCDM @?= V2 (-5.0) (-0.0)
    pRCM @?= V2 (-5.0) (6.0)

  , testCase "Midfield five becomes a midfield four when one player is pressing" $ do
    let ssc = 
          ShapeSpecContext 
            { shapeSpecContextPressingPlayer = Just playerLCM
            }

    (pLWB, pCDM, pRCM, pRWB) <- runTestMLR ssc $ do
      pLW <- outOfPossessionFormationRelativeTo (teamFormation threeFiveTwoTeam) 1 0 playerLWB (V2 0 0)
      pCDM <- outOfPossessionFormationRelativeTo (teamFormation threeFiveTwoTeam) 1 0 playerDM (V2 0 0)
      pRCM <- outOfPossessionFormationRelativeTo (teamFormation threeFiveTwoTeam) 1 0 playerRCM (V2 0 0)
      pRW <- outOfPossessionFormationRelativeTo (teamFormation threeFiveTwoTeam) 1 0 playerRWB (V2 0 0)
      pure (pLW, pCDM, pRCM, pRW)

    pLWB @?= V2 (-5.0) (-9.0)
    pCDM @?= V2 (-5.0) (-3.0)
    pRCM @?= V2 (-5.0) (3.0)
    pRWB @?= V2 (-5.0) (9.0)
  
  , testCase "Back four becomes a back three when one player is pressing" $ do
    let ssc = 
          ShapeSpecContext 
            { shapeSpecContextPressingPlayer = Just playerRFB
            }

    (pLFB, pLCB, pRCB) <- runTestMLR ssc $ do
      
      pLFB <- outOfPossessionFormationRelativeTo (teamFormation fourThreeThreeTeam) 1 0 playerLFB (V2 0 0)
      pLCB <- outOfPossessionFormationRelativeTo (teamFormation fourThreeThreeTeam) 1 0 playerLCB (V2 0 0)
      pRCB <- outOfPossessionFormationRelativeTo (teamFormation fourThreeThreeTeam) 1 0 playerRCB (V2 0 0)
      
      pure (pLFB, pLCB, pRCB)

    
    pLFB @?= V2 (-15.0) (-6.0)
    pLCB @?= V2 (-15.0) (-0.0)
    pRCB @?= V2 (-15.0) (6.0)
  
  , testCase "Midfield three becomes a midfield two when one player is pressing" $ do
    let ssc = 
          ShapeSpecContext 
            { shapeSpecContextPressingPlayer = Just playerLCM
            }

    (pCDM, pRCM) <- runTestMLR ssc $ do
      
      pCDM <- outOfPossessionFormationRelativeTo (teamFormation fourThreeThreeTeam) 1 0 playerDM (V2 0 0)
      pRCM <- outOfPossessionFormationRelativeTo (teamFormation fourThreeThreeTeam) 1 0 playerRCM (V2 0 0)
      
      pure (pCDM, pRCM)

    
    pCDM @?= V2 (-5.0) (-3.0)
    pRCM @?= V2 (-5.0) (3.0)

  , testCase "Front two are properly positioned" $ do
    let ssc = 
          ShapeSpecContext 
            { shapeSpecContextPressingPlayer = Nothing
            }

    (pLCF, pRCF) <- runTestMLR ssc $ do
      
      pLCF <- outOfPossessionFormationRelativeTo (teamFormation threeFiveTwoTeam) 1 0 playerLCF (V2 0 0)
      pRCF <- outOfPossessionFormationRelativeTo (teamFormation threeFiveTwoTeam) 1 0 playerRCF (V2 0 0)
      
      pure (pLCF, pRCF)

    
    pLCF @?= V2 (10.0) (-3.0)
    pRCF @?= V2 (10.0) (3.0)

  , testCase "Front two becomes a front one when one player is pressing" $ do
    let ssc = 
          ShapeSpecContext 
            { shapeSpecContextPressingPlayer = Just playerRCF
            }

    pRCF <- runTestMLR ssc $ do
      pRCF <- outOfPossessionFormationRelativeTo (teamFormation threeFiveTwoTeam) 1 0 playerLCF (V2 0 0)
      pure pRCF

    
    pRCF @?= V2 (10.0) (0.0)
    
  ]

