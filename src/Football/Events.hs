module Football.Events where
import Football.Types
import Football.Match (Match (matchEventLog))
import Data.List (scanl', scanr, groupBy)
import Data.Maybe (mapMaybe, catMaybes)

data PlayElement
  = PassElement Pass
  | CarryElement [TouchOfBall]

data PassageOfPlay = PassageOfPlay 
  { passageOfPlayTouches :: [TouchOfBall]
  , passageOfPlayTeam :: TeamId
  , passageOfPlayElements :: [PlayElement]
  }

touchEvents :: (Match m, Monad m) => m [TouchOfBall]
touchEvents = do
  concatMap f <$> matchEventLog
  where
    f (TouchLogEntry t) = [t]
    f _                 = []

turnovers :: (Match m, Monad m) => m [TouchOfBall]
turnovers = snd . foldr folder (Nothing, []) <$> matchEventLog
  where
    folder (TouchLogEntry t) (Just p, xs) | playerTeamId p /= playerTeamId (touchOfBallPlayer t) = 
      let player = touchOfBallPlayer t
      in (Just player, t : xs)
    folder (TouchLogEntry t) (Nothing, xs)  = 
      let player = touchOfBallPlayer t
      in (Just player, xs)
    folder  _ (c, xs) = (c, xs)

passes :: (Match m, Monad m) => m [Pass]
passes = 
  concatMap (mapMaybe mapper . passageOfPlayElements) <$> passagesOfPlay
  where 
     mapper (PassElement pass) = Just pass
     mapper _                  = Nothing

passagesOfPlay :: (Match m, Monad m) => m [PassageOfPlay]
passagesOfPlay = do
  passages <- mapMaybe (toPassage . mapMaybe mapper) . groupBy grouper . reverse <$> matchEventLog
  pure $ fmap mapPassage passages
  where
    grouper e1 e2 | entryTeam e1 == entryTeam e2 = True
    grouper _ _ = False

    entryTeam (TouchLogEntry t1) = playerTeamId $ touchOfBallPlayer t1
    entryTeam (GoalLogEntry goal) = goalTeam goal
    entryTeam (RestartLogEntry (CornerKick team _)) = team
    entryTeam (RestartLogEntry (ThrowIn team _)) = team
    entryTeam (RestartLogEntry (GoalKick team _)) = team
    entryTeam (RestartLogEntry (KickOff team)) = team

    mapper (TouchLogEntry t) = Just t
    mapper _                 = Nothing

    toPassage (t:ts) = Just $ PassageOfPlay { passageOfPlayTouches = t : ts, passageOfPlayTeam = playerTeamId $ touchOfBallPlayer t, passageOfPlayElements = [] }
    toPassage [] = Nothing

    extractPassesAndCarries t (Just nt) =
      case (touchOfBallType t, touchOfBallType nt) of
        (PassTouch, _) -> Just $ PassElement (CompletePass t nt)
        (DribbleTouch, _) -> Just $ CarryElement [t]
        (_, _) -> Nothing
    extractPassesAndCarries t Nothing =
      case touchOfBallType t of
        PassTouch -> Just $ PassElement (IncompletePass t)
        DribbleTouch -> Just $ CarryElement [t]
        _ -> Nothing

    combineCarries (CarryElement ts) (Just (CarryElement [t])) = CarryElement (t:ts)
    combineCarries e _ = e

    mapPassage passage = passage { passageOfPlayElements = zipWithMaybeNext combineCarries $ catMaybes $ zipWithMaybeNext extractPassesAndCarries $ passageOfPlayTouches passage }


    zipWithMaybeNext f xs = zipWith f xs $ fmap Just (drop 1 xs) ++ repeat Nothing
