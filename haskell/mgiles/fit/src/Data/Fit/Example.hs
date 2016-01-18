module Data.Fit.Example (
  fitSpeeds,
  fitFields,
  readEntries,
  degrees,
  semicircles,
  cmToM,
  msToMPH
) where

import           Control.Lens
import           Data.List
import           Data.Time
import           Data.Time
import           Data.Time.Clock.POSIX

import           Fit

data Entry = Entry {
  lat        :: Int,           -- latitude in semicircles
  latDeg     :: Double,        -- latitude in degrees
  lon        :: Int,           -- longitude in semicircles
  lonDeg     :: Double,        -- longitude in degrees
  ts         :: Int,           -- scaled unix timestamp
  tsUTC      :: UTCTime,       -- proper unix timestamp
  distance   :: Int,           -- scaled distance
  distanceCM :: Double,        -- distance in cm
  speed      :: Int,           -- scaled speed
  speedMS    :: Double,        -- speed in m/s
  speedMPH   :: Double         -- speed in mph
} deriving (Show, Read, Eq)



degrees semicircles = semicircles * (180 / 2^31)
semicircles degrees = degrees * (2^31 / 180)
cmToM = (*) 0.01
msToMPH = (*) 2.23693629



fitSpeeds path = do
  Right fit <- readFileMessages path
  let speeds = fit ^.. message 20 . field 6 . int
  return speeds



fitFields path f = do
  Right fit <- readFileMessages path
  let fs = fit ^.. message 20 . field f . int
  return fs



readEntries path = do
  Right fit <- readFileMessages path
  let
    timestamps = fit ^.. message 20 . field 253 . int
    latitudes = fit ^.. message 20 . field 0 . int
    longitudes = fit ^.. message 20 . field 1 . int
    distances = fit ^.. message 20 . field 5 . int
    speeds = fit ^.. message 20 . field 6 . int
    everything = zip5 timestamps latitudes longitudes distances speeds
  return $ map (\(ts,lat,lon,dist,speed) -> Entry {
        ts = ts,
        tsUTC = posixSecondsToUTCTime (fromIntegral ts),
        lat = lat,
        latDeg = degrees $ fromIntegral lat,
        lon = lon,
        lonDeg = degrees $ fromIntegral lon,
        distance = dist,
        distanceCM = (fromIntegral dist) / 100,
        speed = speed,
        speedMS = (fromIntegral speed) / 1000,
        speedMPH = msToMPH ((fromIntegral speed) / 1000)
      }) everything



-- stuff
-- liftM (sum . map distanceCM) $ readEntries "/code/GARMIN/GARMIN/ACTIVITY/61G90344.FIT"
