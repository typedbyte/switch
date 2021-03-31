{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
module Device.Nintendo.Switch.Controller where

-- attoparsec
import Data.Attoparsec.ByteString (IResult(Done), Parser, anyWord8, parse, take, word8)

-- base
import Data.Bits      ((.&.), (.|.), Bits, shiftL, shiftR)
import Data.Int       (Int16)
import Data.IORef     (IORef)
import Data.Word      (Word8, Word16)
import Prelude hiding (take)

-- bytestring
import Data.ByteString (ByteString)

-- hidapi
import System.HIDAPI (Device)

-- switch
import Device.Nintendo.Switch.Utils (tripleParser, tripleZipWith)

-- | The types of Nintendo Switch controllers that are currently supported by
-- this library.
--
-- Note that this type is mostly used on the type level (using @DataKinds@)
-- in order to prevent programming mistakes at compile-time (e.g., to prevent
-- sending a rumble command to a controller which has no rumble feature).
--
-- Chances are very high that you don't need this type on the value level, but
-- rather on the type level, for example via @TypeApplications@
-- (see 'Device.Nintendo.Switch.getControllerInfos').
data ControllerType
  = LeftJoyCon
  | RightJoyCon
  | ProController
  deriving (Eq, Read, Show)

-- | A handle which represents a connected Nintendo Switch controller.
data Controller (t :: ControllerType) =
  Controller
    { handle      :: Device
    , counter     :: IORef Word8
    , calibration :: Calibration
    }

data Calibration =
  Calibration
    { accCoeffs     :: (Float, Float, Float)
    , gyroCoeffs    :: (Float, Float, Float)
    , leftStickCal  :: StickCalibration
    , rightStickCal :: StickCalibration
    }
  deriving (Eq, Read, Show)

data RawSensorCalibration =
  RawSensorCalibration
    { rawAcc             :: (Int16, Int16, Int16)
    , rawAccSensitivity  :: (Int16, Int16, Int16)
    , rawGyro            :: (Int16, Int16, Int16)
    , rawGyroSensitivity :: (Int16, Int16, Int16)
    }
  deriving (Eq, Read, Show)

data RawStickCalibration =
  RawStickCalibration
    { rawMinusX  :: Word16
    , rawCenterX :: Word16
    , rawPlusX   :: Word16
    , rawMinusY  :: Word16
    , rawCenterY :: Word16
    , rawPlusY   :: Word16
    }
  deriving (Eq, Read, Show)

data RawCalibration =
  RawCalibration
    { rawLeftStickCal  :: RawStickCalibration
    , rawRightStickCal :: RawStickCalibration
    , rawSensorCal     :: RawSensorCalibration
    }
  deriving (Eq, Read, Show)

data StickCalibration =
  StickCalibration
    { deadCenter :: Float
    , deadOuter  :: Float
    , minusX     :: Float
    , centerX    :: Float
    , plusX      :: Float
    , minusY     :: Float
    , centerY    :: Float
    , plusY      :: Float
    }
  deriving (Eq, Read, Show)

-- | A constraint which indicates that a Nintendo Switch controller is able to
-- turn portions of its internal flash memory into valid calibration information.
class HasCalibration (t :: ControllerType) where
  calibrate :: RawCalibration -> Calibration

instance HasCalibration 'LeftJoyCon where
  calibrate (RawCalibration l r s) =
    Calibration
      { accCoeffs     = tripleZipWith toAccCoeff (rawAcc s) (rawAccSensitivity s)
      , gyroCoeffs    = tripleZipWith toGyroCoeff (rawGyro s) (rawGyroSensitivity s)
      , leftStickCal  = toStickCal 0.15 0.10 l
      , rightStickCal = toStickCal 0.15 0.10 r
      }

instance HasCalibration 'RightJoyCon where
  calibrate (RawCalibration l r s) =
    Calibration
      { accCoeffs     = tripleZipWith toAccCoeff (rawAcc s) (rawAccSensitivity s)
      , gyroCoeffs    = tripleZipWith toGyroCoeff (rawGyro s) (rawGyroSensitivity s)
      , leftStickCal  = toStickCal 0.15 0.10 l
      , rightStickCal = toStickCal 0.15 0.10 r
      }

instance HasCalibration 'ProController where
  calibrate (RawCalibration l r s) =
    Calibration
      { accCoeffs     = tripleZipWith toAccCoeff (rawAcc s) (rawAccSensitivity s)
      , gyroCoeffs    = tripleZipWith toGyroCoeff (rawGyro s) (rawGyroSensitivity s)
      , leftStickCal  = toStickCal 0.10 0.10 l
      , rightStickCal = toStickCal 0.10 0.10 r
      }

toAccCoeff :: Int16 -> Int16 -> Float
toAccCoeff sense value =
  (1 / (fromIntegral sense - fromIntegral value)) * 4 -- in Gs

toGyroCoeff :: Int16 -> Int16 -> Float
toGyroCoeff sense value =
  (936 / (fromIntegral sense - fromIntegral value)) * (pi / 180) -- in radians per second

toStickCal :: Float -> Float -> RawStickCalibration -> StickCalibration
toStickCal dc dout raw =
  StickCalibration
    { deadCenter = dc
    , deadOuter  = dout
    , minusX     = fromIntegral $ rawMinusX raw
    , centerX    = fromIntegral $ rawCenterX raw
    , plusX      = fromIntegral $ rawPlusX raw
    , minusY     = fromIntegral $ rawMinusY raw
    , centerY    = fromIntegral $ rawCenterY raw
    , plusY      = fromIntegral $ rawPlusY raw
    }

axisCalibrationParser :: Parser RawSensorCalibration
axisCalibrationParser = do
  acc       <- tripleParser
  accSense  <- tripleParser
  gyro      <- tripleParser
  gyroSense <- tripleParser
  pure $
    RawSensorCalibration
      { rawAcc             = acc
      , rawAccSensitivity  = accSense
      , rawGyro            = gyro
      , rawGyroSensitivity = gyroSense
      }

axisUserParser :: Parser RawSensorCalibration
axisUserParser = do
  _ <- word8 0xB2
  _ <- word8 0xA1
  axisCalibrationParser

parseStickUserCalibration :: ByteString -> (Maybe RawStickCalibration, Maybe RawStickCalibration)
parseStickUserCalibration string =
  case parse stickUserParser string of
    Done _ result -> result
    _             -> (Nothing, Nothing)

stickCalibrationParser :: (Bits a, Num a) => Parser (a, a, a, a, a, a)
stickCalibrationParser = do
  byte0 <- fromIntegral <$> anyWord8
  byte1 <- fromIntegral <$> anyWord8
  byte2 <- fromIntegral <$> anyWord8
  byte3 <- fromIntegral <$> anyWord8
  byte4 <- fromIntegral <$> anyWord8
  byte5 <- fromIntegral <$> anyWord8
  byte6 <- fromIntegral <$> anyWord8
  byte7 <- fromIntegral <$> anyWord8
  byte8 <- fromIntegral <$> anyWord8
  let d0 = shiftL byte1 8 .&. 0x0F00 .|. byte0
      d1 = shiftL byte2 4 .|. shiftR byte1 4
      d2 = shiftL byte4 8 .&. 0x0F00 .|. byte3
      d3 = shiftL byte5 4 .|. shiftR byte4 4
      d4 = shiftL byte7 8 .&. 0x0F00 .|. byte6
      d5 = shiftL byte8 4 .|. shiftR byte7 4
  pure (d0, d1, d2, d3, d4, d5)

leftStickCalibrationParser :: Parser RawStickCalibration
leftStickCalibrationParser = do
  (d0, d1, d2, d3, d4, d5) <- stickCalibrationParser
  pure $
    RawStickCalibration
      { rawMinusX  = d2 - d4
      , rawCenterX = d2
      , rawPlusX   = d2 + d0
      , rawMinusY  = d3 - d5
      , rawCenterY = d3
      , rawPlusY   = d3 + d1
      }

rightStickCalibrationParser :: Parser RawStickCalibration
rightStickCalibrationParser = do
  (d0, d1, d2, d3, d4, d5) <- stickCalibrationParser
  pure $
    RawStickCalibration
      { rawMinusX  = d0 - d2
      , rawCenterX = d0
      , rawPlusX   = d0 + d4
      , rawMinusY  = d1 - d3
      , rawCenterY = d1
      , rawPlusY   = d1 + d5
      }

stickFactoryParser :: Parser (RawStickCalibration, RawStickCalibration)
stickFactoryParser = do
  leftCal <- leftStickCalibrationParser
  rightCal <- rightStickCalibrationParser
  pure (leftCal, rightCal)

stickUserParser :: Parser (Maybe RawStickCalibration, Maybe RawStickCalibration)
stickUserParser = do
  magicL0 <- anyWord8
  magicL1 <- anyWord8
  ls <- if magicL0 == 0xB2 && magicL1 == 0xA1
        then Just <$> leftStickCalibrationParser
        else const Nothing <$> take 9
  magicR0 <- anyWord8
  magicR1 <- anyWord8
  rs <- if magicR0 == 0xB2 && magicR1 == 0xA1
        then Just <$> rightStickCalibrationParser
        else pure Nothing
  pure (ls, rs)