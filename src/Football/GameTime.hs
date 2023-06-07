module Football.GameTime where
import Football.Types (GameTime(..))

gameTimeSeconds :: GameTime -> Double
gameTimeSeconds (GameTime _ time) =
  fromIntegral time / 1000000
