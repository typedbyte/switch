{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
module Device.Nintendo.Switch.Input where

-- attoparsec
import Data.Attoparsec.ByteString (IResult(Done), Parser, anyWord8, parse, take, word8)

-- base
import Control.Applicative ((<|>))
import Control.Exception   (Exception, throwIO)
import Data.Bits           ((.&.), (.|.), shiftL, shiftR)
import Data.IORef          (IORef)
import Data.Int            (Int16)
import Data.List           (intercalate)
import Data.Word           (Word8, Word16, Word32)
import Numeric             (showHex)
import Prelude      hiding (Left, Right, read, take)

-- bytestring
import qualified Data.ByteString as BS

-- hidapi
import System.HIDAPI (Device, read, readTimeout)

-- switch
import Device.Nintendo.Switch.Controller
import Device.Nintendo.Switch.Output     (requestRawSPI)
import Device.Nintendo.Switch.Utils      (checkMask, clamp, tripleMap, tripleParser, tripleZipWith)

-- | A constraint which indicates that a Nintendo Switch controller can provide
-- t'Input' (see 'getInput').
class HasInput t where
  convert :: Controller t -> RawInput -> Input

instance HasInput 'LeftJoyCon where
  convert controller raw =
    noInput
      { btnL         = btnL raw
      , btnZL        = btnZL raw
      , btnMinus     = btnMinus raw
      , btnLeftStick = btnLeftStick raw
      , btnUp        = btnUp raw
      , btnLeft      = btnLeft raw
      , btnRight     = btnRight raw
      , btnDown      = btnDown raw
      , btnCapture   = btnCapture raw
      , btnLeftSL    = btnLeftSL raw
      , btnLeftSR    = btnLeftSR raw
      , stickLeft    = adjustStick (stickLeft raw) (leftStickCal cal)
      , extras       = adjustSensor (extras raw) (accCoeffs cal) (gyroCoeffs cal)
      , battery      = battery raw
      }
    where
      cal = calibration controller

instance HasInput 'RightJoyCon where
  convert controller raw =
    noInput
      { btnR          = btnR raw
      , btnZR         = btnZR raw
      , btnPlus       = btnPlus raw
      , btnX          = btnX raw
      , btnY          = btnY raw
      , btnA          = btnA raw
      , btnB          = btnB raw
      , btnRightStick = btnRightStick raw
      , btnHome       = btnHome raw
      , btnRightSL    = btnRightSL raw
      , btnRightSR    = btnRightSR raw
      , stickRight    = adjustStick (stickRight raw) (rightStickCal cal)
      , extras        = extraMap
                          ( \(x,y,z) -> (x, -y, -z) ) -- compensate the reverse-installed IMU chip
                          ( adjustSensor (extras raw) (accCoeffs cal) (gyroCoeffs cal) )
      , battery       = battery raw
      }
    where
      cal = calibration controller
      extraMap f = \case
        Inertial a g -> Inertial (tripleMap f a) (tripleMap f g)
        other        -> other
  
instance HasInput 'ProController where
  convert controller raw =
    raw
      { stickLeft  = adjustStick (stickLeft raw) (leftStickCal cal)
      , stickRight = adjustStick (stickRight raw) (rightStickCal cal)
      , extras     = adjustSensor (extras raw) (accCoeffs cal) (gyroCoeffs cal)
      }
    where
      cal = calibration controller

-- | An 'InputException' is thrown if something goes wrong with 'getInput'.
data InputException
  = NoReplyException
    -- ^ Indicates that an expected reply wasn't received in a specific time interval.
  | UnknownFormatException BS.ByteString
    -- ^ Indicates that the controller input has an unexpected format. It essentially
    -- means that a specific part of the protocol has not been implemented yet. This
    -- should not occur as long as you stick to the public API of this library.
  deriving Eq

instance Exception InputException
instance Show InputException where
  show = \case
    NoReplyException ->
      "Did not receive a reply in a given time interval."
    UnknownFormatException bs ->
      "Encountered an unknown input format: "
        ++ (intercalate " " $ fmap (flip showHex "") (BS.unpack bs))

-- | The input provided by a Nintendo Switch controller.
type Input = ControllerInput Float Float

type RawInput = ControllerInput Word16 Int16

-- | The input provided by a Nintendo Switch controller, where @s@ is the
-- numeric type of the analog stick direction and @e@ is the numeric type
-- of the sensor readings (i.e., accelerometer and gyroscope).
data ControllerInput s e =
  Input
    { -- left buttons
      btnL          :: Bool
    , btnZL         :: Bool
    , btnMinus      :: Bool
    , btnLeftStick  :: Bool
    , btnUp         :: Bool
    , btnLeft       :: Bool
    , btnRight      :: Bool
    , btnDown       :: Bool
    , btnCapture    :: Bool
    , btnLeftSL     :: Bool
    , btnLeftSR     :: Bool
      -- right buttons
    , btnR          :: Bool
    , btnZR         :: Bool
    , btnPlus       :: Bool
    , btnX          :: Bool
    , btnY          :: Bool
    , btnA          :: Bool
    , btnB          :: Bool
    , btnRightStick :: Bool
    , btnHome       :: Bool
    , btnRightSL    :: Bool
    , btnRightSR    :: Bool
      -- sticks
    , stickLeft     :: StickDirection s
    , stickRight    :: StickDirection s
      -- others
    , extras        :: ExtraInput e
    , battery       :: Maybe BatteryInfo
    }
  deriving (Eq, Read, Show)

-- | A convenient constant that represents no input. This can be used to set
-- specific buttons and stick directions in order to test functions without
-- having a Nintendo Switch controller at hand, like:
--
-- @
--     'noInput' { 'btnX' = 'True', 'stickLeft' = 'Discrete' 'Up' }
-- @
-- 
noInput :: ControllerInput s e
noInput =
  Input
    { btnL          = False
    , btnZL         = False
    , btnMinus      = False
    , btnLeftStick  = False
    , btnUp         = False
    , btnLeft       = False
    , btnRight      = False
    , btnDown       = False
    , btnCapture    = False
    , btnLeftSL     = False
    , btnLeftSR     = False
    , btnR          = False
    , btnZR         = False
    , btnPlus       = False
    , btnX          = False
    , btnY          = False
    , btnA          = False
    , btnB          = False
    , btnRightStick = False
    , btnHome       = False
    , btnRightSL    = False
    , btnRightSR    = False
    , stickLeft     = Discrete None
    , stickRight    = Discrete None
    , extras        = Unavailable
    , battery       = Nothing
    }

-- | Accelerometer data consists of three measurements recorded in 15ms (i.e.,
-- the precision is 5ms). Each measurement is an x\/y\/z triple measured in Gs.
type Accelerometer a = ((a, a, a), (a, a, a), (a, a, a))

-- | Gyroscope data consists of three measurements recorded in 15ms (i.e., the
-- precision is 5ms). Each measurement is an x\/y\/z triple measured in radians
-- per second.
type Gyroscope a = ((a, a, a), (a, a, a), (a, a, a))

-- | Depending on the t'Device.Nintendo.Switch.InputMode', v'Input' can contain
-- additional information: Replies to commands (e.g., an acknowledgement when
-- sending a rumble command) and inertial sensor data (i.e., accelerometer and
-- gyroscope).
data ExtraInput a
  = CommandReply ReplyData
    -- ^ After sending commands to the controller (e.g., setting the
    -- t'Device.Nintendo.Switch.InputMode'), a command reply is returned as extra
    -- data in the next input.
  | Inertial (Accelerometer a) (Gyroscope a)
    -- ^ A controller provides inertial sensor data (i.e., accelerometer and
    -- gyroscope) only if it is in 'Device.Nintendo.Switch.Standard' input mode and
    -- inertial measurement is activated via 'Device.Nintendo.Switch.setInertialMeasurement'.
    --
    -- Regarding the x\/y\/z coordinate system, consider the left Joy-Con lying
    -- flat on a table, the analog stick pointing up. The x-axis then points
    -- towards the Z/ZL shoulder buttons (or alternatively: to where the up arrow
    -- button is pointing), the y-axis points to the opposite side of the SL/SR
    -- buttons (or alternatively: to where the left arrow button is pointing),
    -- and the z-axis points up in the air. The coordinate system is the same for
    -- all controller types.
  | Unavailable
    -- ^ Indicates that there is no additional input data.
  deriving (Eq, Functor, Read, Show)

-- | Whenever a command is sent to a controller (e.g., setting the
-- t'Device.Nintendo.Switch.InputMode'), the controller replies with an
-- 'Acknowledgement'.
data Acknowledgement a
  = ACK a -- ^ The command was executed successfully, possibly holding some response
          -- data (e.g., if the command was a query of the internal SPI flash memory).
  | NACK  -- ^ The command was not executed successfully.
  deriving (Eq, Functor, Read, Show)

-- | Data type that combines the command type and its corresponding acknowledgement.
data ReplyData
  = RequestSPI             (Acknowledgement (Word32, Word8, BS.ByteString))
  | SetHomeLight           (Acknowledgement ())
  | SetInertialMeasurement (Acknowledgement ())
  | SetInputMode           (Acknowledgement ())
  | SetPlayerLights        (Acknowledgement ())
  | SetVibration           (Acknowledgement ())
  | UnknownCommand         Word8 Word8
  deriving (Eq, Read, Show)

-- | The direction of the left ('stickLeft') and right ('stickRight') analog sticks.
data StickDirection a
  = Discrete Direction
    -- ^ In 'Device.Nintendo.Switch.Simple' input mode, controllers send discrete
    -- stick directions.
  | Analog a a
    -- ^ In 'Device.Nintendo.Switch.Standard' input mode, controllers send analog
    -- stick directions. The first value is left/right (interval @[-1,1]@), the
    -- second value is down/up (interval @[-1,1]@).
  deriving (Eq, Functor, Read, Show)

-- | The nine possible discrete positions of the analog stick in
-- 'Device.Nintendo.Switch.Simple' input mode.
data Direction
  = None
  | Left
  | Up
  | Right
  | Down
  | LeftUp
  | LeftDown
  | RightUp
  | RightDown
  deriving (Eq, Read, Show)

-- | Converts stick directions into x\/y coordinates in the interval @[-1,1]@.
-- 'Analog' values are taken as is, while 'Discrete' directions are converted
-- to their analog counterpart.
coordinates :: StickDirection Float -> (Float, Float)
coordinates (Analog x y) = (x, y)
coordinates (Discrete dir) =
  case dir of
    None      -> (    0,    0)
    Left      -> ( left,    0)
    Up        -> (    0,   up)
    Right     -> (right,    0)
    Down      -> (    0, down)
    LeftUp    -> ( left,   up)
    LeftDown  -> ( left, down)
    RightUp   -> (right,   up)
    RightDown -> (right, down)
  where
    right = cos (pi / 4)
    left  = -right
    up    = sin (pi / 4)
    down  = -up

-- | Merges the inputs of two Nintendo Switch controllers. The resulting input
-- contains the left button states and left analog stick direction from one input,
-- and the right button states and right analog stick direction from the other
-- input. This can be used to unify the inputs of two controllers that belong
-- together (e.g., a pair of left and right Joy-Cons).
--
-- Note that the 'extras' and 'battery' information of the original inputs are
-- discarded in the merged input (they are set to 'Unavailable' and 'Nothing',
-- respectively).
mergeInputs
  :: Input -- ^ The left-side input to be merged.
  -> Input -- ^ The right-side input to be merged.
  -> Input -- ^ The merged input, without 'extras' and 'battery'.
mergeInputs leftInput rightInput =
  leftInput
    { btnR          = btnR rightInput
    , btnZR         = btnZR rightInput
    , btnPlus       = btnPlus rightInput
    , btnX          = btnX rightInput
    , btnY          = btnY rightInput
    , btnA          = btnA rightInput
    , btnB          = btnB rightInput
    , btnRightStick = btnRightStick rightInput
    , btnHome       = btnHome rightInput
    , btnRightSL    = btnRightSL rightInput
    , btnRightSR    = btnRightSR rightInput
    , stickRight    = stickRight rightInput
    , extras        = Unavailable
    , battery       = Nothing
    }

-- | The battery status of a Nintendo Switch controller.
data BatteryStatus
  = Empty
  | Low
  | Medium
  | Good
  | Full
  deriving (Eq, Ord, Read, Show)

-- | Information about the battery of a Nintendo Switch controller. It is only
-- returned by 'getInput' (see 'battery') if the controller sends a command reply
-- or the input mode of the controller is 'Device.Nintendo.Switch.Standard'.
data BatteryInfo =
  BatteryInfo
    { batteryStatus :: BatteryStatus
    , charging      :: Bool
    }
  deriving (Eq, Read, Show)

adjustSensor
  :: ExtraInput Int16
  -> (Float, Float, Float)
  -> (Float, Float, Float)
  -> ExtraInput Float
adjustSensor Unavailable _ _ = Unavailable
adjustSensor (CommandReply r) _ _ = CommandReply r
adjustSensor (Inertial acc gyro) accs gyros =
  Inertial newAcc newGyro
    where facc = tripleMap (tripleMap fromIntegral) acc
          fgyro = tripleMap (tripleMap fromIntegral) gyro
          newAcc = tripleMap (tripleZipWith (*) accs) facc
          newGyro = tripleMap (tripleZipWith (*) gyros) fgyro

readRawInput :: Device -> IO RawInput
readRawInput dev = do
  response <- read dev 362
  case parse inputParser response of
    Done _ raw -> pure raw
    _          -> throwIO $ UnknownFormatException response

-- | Reads input from a Nintendo Switch controller. Blocks until controller
-- input is available.
getInput :: HasInput t => Controller t -> IO Input
getInput controller =
  convert controller <$>
    readRawInput (handle controller)

readRawTimeoutInput :: Int -> Device -> IO (Maybe RawInput)
readRawTimeoutInput timeout dev = do
  response <- readTimeout dev 362 timeout
  if BS.length response <= 0
  then pure Nothing
  else case parse inputParser response of
    Done _ raw -> pure $ Just raw
    _          -> throwIO $ UnknownFormatException response

-- | Reads input from a Nintendo Switch controller. Blocks until controller
-- input is available or a given time interval elapses.
getTimeoutInput
  :: HasInput t
  => Int              -- ^ The time interval in milliseconds.
  -> Controller t     -- ^ The controller to read the input from.
  -> IO (Maybe Input) -- ^ Returns 'Nothing' if the controller does not provide
                      -- an input within the specified time interval.
getTimeoutInput timeout controller =
  fmap (convert controller) <$>
    readRawTimeoutInput timeout (handle controller)

inputParser :: Parser RawInput
inputParser
    = standardParser
  <|> buttonPushParser
  <|> commandParser

commandDetailParser :: Parser ReplyData
commandDetailParser
    = requestSPIReplyParser
  <|> homeLightReplyParser
  <|> vibrationReplyParser
  <|> inertialReplyParser
  <|> inputModeReplyParser
  <|> playerLightsReplyParser
  <|> unknownCommandParser 

ackParser :: Parser Bool
ackParser = checkMask 0x80 <$> anyWord8

toAck :: Bool -> Acknowledgement ()
toAck True  = ACK ()
toAck False = NACK

requestSPIReplyParser :: Parser ReplyData
requestSPIReplyParser = do
  ack <- ackParser
  _ <- word8 0x10
  case ack of
    False -> pure $ RequestSPI NACK
    True  -> do
      byte0 <- fromIntegral <$> anyWord8
      byte1 <- fromIntegral <$> anyWord8
      byte2 <- fromIntegral <$> anyWord8
      byte3 <- fromIntegral <$> anyWord8
      len   <- anyWord8
      value <- take (fromIntegral len)
      let addrByte0 = shiftL byte3 24
          addrByte1 = shiftL byte2 16
          addrByte2 = shiftL byte1 8
          addrByte3 = byte0
          address   = addrByte0 + addrByte1 + addrByte2 + addrByte3
      pure $ RequestSPI (ACK (address, len, value))

homeLightReplyParser :: Parser ReplyData
homeLightReplyParser = do
  ack <- ackParser
  _ <- word8 0x38
  pure $ SetHomeLight (toAck ack)

vibrationReplyParser :: Parser ReplyData
vibrationReplyParser = do
  ack <- ackParser
  _ <- word8 0x48
  pure $ SetVibration (toAck ack)

inertialReplyParser :: Parser ReplyData
inertialReplyParser = do
  ack <- ackParser
  _ <- word8 0x40
  pure $ SetInertialMeasurement (toAck ack)

inputModeReplyParser :: Parser ReplyData
inputModeReplyParser = do
  ack <- ackParser
  _ <- word8 0x03
  pure $ SetInputMode (toAck ack)

playerLightsReplyParser :: Parser ReplyData
playerLightsReplyParser = do
  ack <- ackParser
  _ <- word8 0x30
  pure $ SetPlayerLights (toAck ack)

unknownCommandParser :: Parser ReplyData
unknownCommandParser = do
  byte0 <- anyWord8
  byte1 <- anyWord8
  pure $ UnknownCommand byte0 byte1

standardParser :: Parser RawInput
standardParser = do
  _ <- word8 0x30
  _ <- anyWord8 -- timer byte
  batInfo    <- batteryParser
  btnInput   <- buttonStandardParser noInput
  leftStick  <- rawStickParser
  rightStick <- rawStickParser
  _ <- anyWord8 -- vibration byte
  acc1  <- tripleParser
  gyro1 <- tripleParser
  acc2  <- tripleParser
  gyro2 <- tripleParser
  acc3  <- tripleParser
  gyro3 <- tripleParser
  pure $
    btnInput
      { stickLeft  = leftStick
      , stickRight = rightStick
      , extras     = Inertial (acc1,acc2,acc3) (gyro1,gyro2,gyro3)
      , battery    = Just batInfo
      }

commandParser :: Parser RawInput
commandParser = do
  _ <- word8 0x21
  _ <- anyWord8 -- timer byte
  batInfo    <- batteryParser
  btnInput   <- buttonStandardParser noInput
  leftStick  <- rawStickParser
  rightStick <- rawStickParser
  _ <- anyWord8 -- vibration byte
  cmd <- commandDetailParser
  pure $
    btnInput
      { stickLeft  = leftStick
      , stickRight = rightStick
      , extras     = CommandReply cmd
      , battery    = Just batInfo
      }

buttonPushParser :: Parser RawInput
buttonPushParser = do
  _ <- word8 0x3F
  btnByte1    <- anyWord8
  btnByte2    <- anyWord8
  stickByte   <- anyWord8
  analogStick <- analogParser
  let down  = checkMask 0x01 btnByte1
      right = checkMask 0x02 btnByte1
      left  = checkMask 0x04 btnByte1
      up    = checkMask 0x08 btnByte1
      sl    = checkMask 0x10 btnByte1
      sr    = checkMask 0x20 btnByte1
      lr    = checkMask 0x40 btnByte2
      zlzr  = checkMask 0x80 btnByte2
      (leftStick, rightStick) =
        case analogStick of
          Just (l,r) -> (l,r)
          Nothing ->
            ( case stickByte of
                0x00 -> Discrete Right
                0x01 -> Discrete RightDown
                0x02 -> Discrete Down
                0x03 -> Discrete LeftDown
                0x04 -> Discrete Left
                0x05 -> Discrete LeftUp
                0x06 -> Discrete Up
                0x07 -> Discrete RightUp
                _    -> Discrete None
            , case stickByte of
                0x00 -> Discrete Left
                0x01 -> Discrete LeftUp
                0x02 -> Discrete Up
                0x03 -> Discrete RightUp
                0x04 -> Discrete Right
                0x05 -> Discrete RightDown
                0x06 -> Discrete Down
                0x07 -> Discrete LeftDown
                _    -> Discrete None
            )
  pure $
    Input
      { btnL          = lr
      , btnZL         = zlzr
      , btnMinus      = checkMask 0x01 btnByte2
      , btnLeftStick  = checkMask 0x04 btnByte2
      , btnUp         = left
      , btnLeft       = down
      , btnRight      = up
      , btnDown       = right
      , btnCapture    = checkMask 0x20 btnByte2
      , btnLeftSL     = sl
      , btnLeftSR     = sr
      , btnR          = lr
      , btnZR         = zlzr
      , btnPlus       = checkMask 0x02 btnByte2
      , btnX          = right
      , btnY          = up
      , btnA          = down
      , btnB          = left
      , btnRightStick = checkMask 0x08 btnByte2
      , btnHome       = checkMask 0x10 btnByte2
      , btnRightSL    = sl
      , btnRightSR    = sr
      , stickLeft     = leftStick
      , stickRight    = rightStick
      , extras        = Unavailable
      , battery       = Nothing
      }
  where
    analogParser :: Parser (Maybe (StickDirection Word16, StickDirection Word16))
    analogParser = fillerParser <|> proParser
    fillerParser = do
      _ <- word8 0x00
      _ <- word8 0x80
      _ <- word8 0x00
      _ <- word8 0x80
      _ <- word8 0x00
      _ <- word8 0x80
      _ <- word8 0x00
      _ <- word8 0x80
      pure Nothing
    proParser = do
      left  <- stickBytesParser
      right <- stickBytesParser
      pure $ Just (left, right)
    stickBytesParser = do
      stickByte0 <- fromIntegral <$> anyWord8
      stickByte1 <- fromIntegral <$> anyWord8
      stickByte2 <- fromIntegral <$> anyWord8
      stickByte3 <- fromIntegral <$> anyWord8
      pure $
        Analog
          ( stickByte0 .|. shiftL stickByte1 8 )
          ( stickByte2 .|. shiftL stickByte3 8 )

batteryParser :: Parser BatteryInfo
batteryParser = do
  batteryByte <- anyWord8
  pure $
    BatteryInfo {
      batteryStatus = toStatus batteryByte,
      charging      = checkMask 0x01 batteryByte
    }
  where toStatus b | b <= 1 = Empty
                   | b <= 3 = Low
                   | b <= 5 = Medium
                   | b <= 7 = Good
                   | otherwise = Full

buttonStandardParser :: RawInput -> Parser RawInput
buttonStandardParser raw = do
  rightByte  <- anyWord8
  sharedByte <- anyWord8
  leftByte   <- anyWord8
  pure $
    raw
      { btnLeftStick  = checkMask 0x08 sharedByte
      , btnZL         = checkMask 0x80 leftByte
      , btnL          = checkMask 0x40 leftByte
      , btnMinus      = checkMask 0x01 sharedByte
      , btnUp         = checkMask 0x02 leftByte
      , btnLeft       = checkMask 0x08 leftByte
      , btnRight      = checkMask 0x04 leftByte
      , btnDown       = checkMask 0x01 leftByte
      , btnCapture    = checkMask 0x20 sharedByte
      , btnLeftSL     = checkMask 0x20 leftByte
      , btnLeftSR     = checkMask 0x10 leftByte
      , btnRightStick = checkMask 0x04 sharedByte
      , btnZR         = checkMask 0x80 rightByte
      , btnR          = checkMask 0x40 rightByte
      , btnPlus       = checkMask 0x02 sharedByte
      , btnX          = checkMask 0x02 rightByte
      , btnY          = checkMask 0x01 rightByte
      , btnA          = checkMask 0x08 rightByte
      , btnB          = checkMask 0x04 rightByte
      , btnHome       = checkMask 0x10 sharedByte
      , btnRightSL    = checkMask 0x20 rightByte
      , btnRightSR    = checkMask 0x10 rightByte
      }

rawStickParser :: Parser (StickDirection Word16)
rawStickParser = do
  byte0 <- fromIntegral <$> anyWord8
  byte1 <- fromIntegral <$> anyWord8
  byte2 <- fromIntegral <$> anyWord8
  let x = byte0 .|. shiftL (byte1 .&. 0x0F) 8
      y = shiftR byte1 4 .|. shiftL byte2 4
  pure $ Analog x y

adjustStick :: StickDirection Word16 -> StickCalibration -> StickDirection Float
adjustStick (Discrete dir) _ = Discrete dir
adjustStick (Analog x y) (StickCalibration dc dout mx cx px my cy py) = let
    clampX = clamp mx px (fromIntegral x)
    clampY = clamp my py (fromIntegral y)
    xf = if clampX >= cx
         then (clampX - cx) /  (px - cx)
         else (clampX - cx) / (cx - mx)
    yf = if clampY >= cy
         then (clampY - cy) / (py - cy)
         else (clampY - cy) / (cy - my)
    mag = sqrt (xf * xf + yf * yf)
    legalRange = 1 - dout - dc
    normMag = min 1 ((mag - dc) / legalRange)
    scale = normMag / mag
  in if mag > dc
  then Analog (xf * scale) (yf * scale)
  else Analog 0 0

withRawSPIData :: Device -> IORef Word8 -> Word32 -> Word8 -> (BS.ByteString -> a) -> IO a
withRawSPIData dev ref start len f = do
  requestRawSPI dev ref start len
  withRawCommandReply 10 50 dev $ \case
    RequestSPI (ACK (addr, rLen, value)) | addr == start && rLen == len ->
      Just $ f value
    _ ->
      Nothing

withRawCommandReply :: Int -> Int -> Device -> (ReplyData -> Maybe a) -> IO a
withRawCommandReply count timeout dev f = loop 0
  where
    cCount   = clamp 0 maxBound count
    cTimeout = clamp 0 maxBound timeout
    loop times
      | times == cCount = throwIO NoReplyException
      | otherwise = do
          input <- readRawTimeoutInput cTimeout dev
          case input of
            Just (Input { extras = CommandReply obj }) ->
              case f obj of
                Just r  -> pure r
                Nothing -> loop (succ times)
            _ -> loop (succ times)

-- | Consumes inputs from a Nintendo Switch controller until a specific command
-- reply is encountered. Throws a 'NoReplyException' if the expected command
-- reply is not encountered within a specified count of inputs.
--
-- This function can be used to make sure that the controller is in an expected
-- state after sending commands (e.g., to wait for an 'Acknowledgement' after
-- switching its t'Device.Nintendo.Switch.InputMode').
withCommandReply
  :: Int
     -- ^ The maximum count of inputs that should be consumed.
  -> Int
     -- ^ The timeout per input read (see 'getTimeoutInput').
  -> Controller t
     -- ^ The controller to read the input from.
  -> (ReplyData -> Maybe a)
     -- ^ The function which checks the command reply. It must return 'Nothing'
     -- if a 'ReplyData' is encountered which we are not looking for, or 'Just' @a@
     -- if everything went well.
  -> IO a
     -- ^ The data extracted from the expected command reply.
withCommandReply count timeout controller f =
  withRawCommandReply count timeout (handle controller) f