module Football.Behaviours.Pass where

import Football.Ball
import Football.Player
import Football.Engine
import Linear (normalize, V3 (V3))

safestPassingOptions :: (Monad m, Engine m) => Player -> m [Player]
safestPassingOptions player = do
  teamPlayers' <- teammates player
  oppositionPlayers' <- oppositionPlayers (playerTeam player)
  pure []


