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
turnovers = snd . foldr scanner (Nothing, []) <$> matchEventLog
  where
    scanner (TouchLogEntry t) (Just p, xs) | playerTeam p /= playerTeam (touchOfBallPlayer t) = 
      let player = touchOfBallPlayer t
      in (Just player, t : xs)
    scanner (TouchLogEntry t) (Nothing, xs)  = 
      let player = touchOfBallPlayer t
      in (Just player, xs)
    scanner  _ (c, xs) = (c, xs)





