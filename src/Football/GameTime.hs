module Football.GameTime where
import Football.Types (GameTime(..))

gameTimeSeconds :: GameTime -> Double
gameTimeSeconds (GameTime _ time) =
  fromIntegral time / 1000000

gameTimeAddSeconds :: GameTime -> Double -> GameTime
gameTimeAddSeconds (GameTime h time) ts =
  let ti = floor $ ts * 1000000
  in GameTime h (time + ti)
