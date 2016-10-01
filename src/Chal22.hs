
import System.Random
import System.Process
import Data.Time.Clock.POSIX
import Data.Int
import Chal21
import Control.Concurrent

type Timestamp = Int32
type Seed = Int32

--just spin through all the previous times.
-- Is this what the challenge intended?
seedFromOutput :: Int32 -> Timestamp -> Seed
seedFromOutput output ts =
    let timestamps = previousTimes ts
        isSeed timestamp = firstOutputFromSeed timestamp == output
    in head $ dropWhile (not . isSeed) timestamps

previousTimes :: Timestamp -> [Timestamp]
previousTimes ts = ts: previousTimes (ts - 1)

firstOutputFromSeed :: Seed -> Int32
firstOutputFromSeed seed = fst $ getNextNumber (initialiseMT seed)

lowerBound = 40 * 1000000
upperBound = 500 * 1000000

main = do randTime <- randomRIO(lowerBound, upperBound)
          randTime2 <- randomRIO(lowerBound, upperBound)
          threadDelay randTime
          timestampSeed <- round `fmap` getPOSIXTime
          threadDelay randTime2
          let (randomOutput, _) = getNextNumber (initialiseMT timestampSeed)
          timestampNow <- round `fmap` getPOSIXTime
          print $ "actualSeed: " ++ show timestampSeed
          print $ "guess: " ++ show (seedFromOutput randomOutput timestampNow)
