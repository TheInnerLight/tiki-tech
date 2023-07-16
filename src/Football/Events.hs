module Football.Events where
import Football.Types
import Football.Match (Match (matchEventLog))
import Data.List (scanl', scanr)

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
passes = snd . foldr folder (Nothing, []) <$> matchEventLog
  where
    folder (TouchLogEntry t) (Just p, xs) | playerTeamId p == playerTeamId (touchOfBallPlayer t) =
      let player = touchOfBallPlayer t
      in (Just player, CompletePass p player : xs)
    folder (TouchLogEntry t) (Nothing, xs) | touchOfBallType t == PassTouch  = 
      let player = touchOfBallPlayer t
      in (Just player, xs)
    folder  _ (Just p, xs) = (Nothing, IncompletePass p : xs)
    folder  _ (Nothing, xs) = (Nothing, xs)

