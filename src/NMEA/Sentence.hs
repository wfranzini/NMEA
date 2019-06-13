{-# LANGUAGE OverloadedStrings #-}
module NMEA.Sentence where

import           Control.Applicative ((<|>))
import           Data.Attoparsec.ByteString.Char8
import           Data.Time.Calendar (Day(..))
import           Data.Time.LocalTime (ZonedTime(..))
import           Data.Maybe (catMaybes)

import NMEA.Common
import NMEA.GPGGA
import NMEA.GPRMC
import NMEA.GPGSA
import NMEA.GPGSV

data Sentence = GPMRC Gprmc
              | GPGGA Gpgga
              | GPHDT Gphdt
              | GPGSA Gpgsa
              | GPGSV Gpgsv
              | GPVTG Gpvtg

    -- | Recommended minimum specific GNSS data
data Gprmc = Gprmc
  { _gprmcTimeUTC           :: ZonedTime
  , _gprmcStatus            :: GprmcStatus
  , _gprmcLatitude          :: Latitude
  , _gprmcLongitude         :: Longitude
  , _gprmcSpeedOverGround   :: Knot
  , _gprmcCourseOverGround  :: Maybe Degree
  , _gpmrcDate              :: Day
  , _gpmrcMagneticVariation :: Maybe MagneticVariation
  , _gprmcMode              :: GprmcMode
  }
  deriving (Eq, Show)

  -- | Global Positioning System Fix Data
data Gpgga =
  Gpgga
  { _gpggaTimeUTC                  :: ZonedTime
  , _gpggaLatitude                 :: Latitude
  , _gpggaLongitude                :: Longitude
  , _gpggaGpsQuality               :: PositionFixIndicator
  , _gpggaNumberOfSatellites       :: Int
  , _gpggaHorizDilutionOfPrecision :: Double
  , _gpggaAltitude                 :: Double
  , _gpggaGeoidalSeparation        :: Double
  , _gpggaAgeDifferentialGPSData   :: Double
  , _gpggaDgpsReferenceStation     :: DGPSReferenceStation
  }
  deriving (Eq, Show)


  -- | Heading from True North
data Gphdt = Gphdt
  { _gphdtHeadingInDegrees :: Degree
  }
  deriving (Eq, Show)


-- | GPS DOP and active satellites
data Gpgsa = Gpgsa
  { _gpgsaMode          :: GPGSAMode
  , _gpgsaPositionFix   :: PositionFix
  , _gpgsaSatellitenPRN :: [SatellitePRN]
  , _gpgsaPDOP          :: PDOP
  , _gpgsaHDOP          :: HDOP
  , _gpgsaVDOP          :: VDOP
  }
  deriving (Eq, Show)


-- | GPS satellites in view
data Gpgsv = Gpgsv
  { _gpgsvTotalMessage :: Int
  , _gpgsvMessageNumber :: Int
  , _gpgsvNumberOfSatellitesInView :: Int
  , _gpgsvSatellitesInView :: [SatelliteInView]
  }
  deriving (Eq, Show)

  -- | Track made good and ground speed
data Gpvtg = Gpvtg
  { -- | true north bearing
    _gpvtgCourseTrue :: Maybe TrueBearing
  -- | magnetic bearing
  , _gpvtgCourseMagnetic  :: Maybe MagneticBearing
  -- | Speed over ground in knots
  , _gpvtgSpeedKnots   :: Knot
  -- | Speed over ground in kilometer per hour
  , _gpvtgSpeedKph     :: Kmh
  }
  deriving (Eq, Show)

sentence :: Century -> Parser Sentence
sentence century =
      GPGGA <$> gpgga
  <|> GPMRC <$> gprmc century
  <|> GPHDT <$> gphdt
  <|> GPGSA <$> gpgsa
  <|> GPGSV <$> gpgsv
  <|> GPVTG <$> gpvtg

gprmc :: Century -> Parser Gprmc
gprmc cen = do
  _       <- string "$GPRMC,"
  time <- timeUTC
  _    <- comma
  stat <- gprmcStatus
  _    <- comma
  lat  <- latitude
  _    <- comma
  lon  <- longitude
  _    <- comma
  spd  <- knot
  _    <- comma
  deg  <- degree'
  _    <- comma
  date <- day cen
  _    <- comma
  mv   <- magneticVariation
  _    <- comma
  mode <- gprmcMode
  _    <- checksum
  return $ Gprmc time stat lat lon spd deg date mv mode

gpgga :: Parser Gpgga
gpgga = do
  string "$GPGGA,"
  time <- timeUTC
  _    <- comma
  lat  <- latitude
  _    <- comma
  lon  <- longitude
  _    <- comma
  qual <- positionFixIndicator
  _    <- comma
  nsat <- decimal :: Parser Int
  _    <- comma
  dilu <- double
  _    <- comma
  alti <- double
  _    <- comma *> char 'M' *> ","
  geoi <- option 0 double
  _    <- comma *> option 'M' (char 'M') *> comma
  age  <- option 0 double
  _    <- comma
  dgps <- option (DGPSReferenceStation 0) dgpsReferenceStation
  _    <- checksum
  return (Gpgga time lat lon qual nsat dilu alti geoi age dgps) <?> "GPPGA"

gphdt :: Parser Gphdt
gphdt = do
  string "$GPHDT"
  _    <- comma
  deg  <- degree
  _    <- comma <* char 'T' <* checksum
  return $ Gphdt deg

gpgsa :: Parser Gpgsa
gpgsa = do
  string "$GPGSA"
  _     <- comma
  mode  <- gpgsaMode
  _     <- comma
  fix   <- positionFix
  _     <- comma
  sats  <- catMaybes <$> count 12 ((option Nothing $ Just <$> satellitePRN) <* comma)
  pdop' <- pdop
  _     <- comma
  hdop' <- hdop
  _     <- comma
  vdop' <- vdop
  _     <- checksum
  return $ Gpgsa mode fix  sats pdop' hdop' vdop'

gpgsv :: Parser Gpgsv
gpgsv = do
  string "$GPGSV"
  _ <- comma
  totalMsg  <- decimal
  _         <- comma
  msgNumber <- decimal
  _         <- comma
  inView    <- decimal
  sats      <- catMaybes <$> count 4 (comma *> satelliteInView)
  _         <- checksum
  return $ Gpgsv totalMsg msgNumber inView sats

gpvtg :: Parser Gpvtg
gpvtg = do
  string "$GPVTG"
  _     <- comma
  true  <- option Nothing $ Just . TrueBearing <$> degreesMinutes
  _     <- comma *> char 'T' *> comma
  magn  <- option Nothing $ Just . MagneticBearing <$> degreesMinutes
  _     <- comma *> char 'M' *> comma
  knot' <- knot
  _     <- comma *> char 'N' *> comma
  kmh'  <- kmh <* comma <* char 'K'
  _     <- checksum
  return $ Gpvtg true magn knot' kmh'
