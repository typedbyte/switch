{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds  #-}
module Device.Nintendo.Switch.Output where

-- base
import Control.Exception (Exception, throwIO)
import Control.Monad     (join, when)
import Data.Bits         ((.|.), (.&.), shiftL, shiftR)
import Data.IORef        (IORef, readIORef, writeIORef)
import Data.Word         (Word8, Word32)

-- bytestring
import qualified Data.ByteString as BS

-- hidapi
import System.HIDAPI (Device, write)

-- switch
import Device.Nintendo.Switch.Controller (Controller(..), ControllerType(..))
import Device.Nintendo.Switch.Utils      (clamp, combine, discretize, pairs)

-- | A constraint which indicates that a Nintendo Switch controller has a home
-- light (see 'setHomeLight').
class HasHomeLight t

instance HasHomeLight 'RightJoyCon
instance HasHomeLight 'ProController

-- | A constraint which indicates that a Nintendo Switch controller has a left-side
-- rumble unit (see 'setLeftRumble').
class HasLeftRumble t

instance HasLeftRumble 'LeftJoyCon
instance HasLeftRumble 'ProController

-- | A constraint which indicates that a Nintendo Switch controller has a right-side
-- rumble unit (see 'setRightRumble').
class HasRightRumble t

instance HasRightRumble 'RightJoyCon
instance HasRightRumble 'ProController

-- | A constraint which indicates that a Nintendo Switch controller has player lights
-- (i.e., the four LEDs which represent the player number; see 'setPlayerLights').
class HasPlayerLights t

instance HasPlayerLights 'LeftJoyCon
instance HasPlayerLights 'RightJoyCon
instance HasPlayerLights 'ProController

-- | A constraint which indicates that a Nintendo Switch controller supports multiple
-- input modes (see 'setInputMode').
class HasInputMode t

instance HasInputMode 'LeftJoyCon
instance HasInputMode 'RightJoyCon

-- | An 'OutputException' is thrown if something goes wrong when sending commands to
-- a Nintendo Switch controller.
data OutputException = WriteException

instance Exception OutputException
instance Show OutputException where
  show WriteException = "Could not send all data to the controller device."

-- | The base duration of a home light configuration in milliseconds. It will
-- always be limited to an interval between 8ms and 175ms. It is called base
-- duration because it will be multiplied with other factors in order to obtain
-- the overall durations of fadings within home light configurations.
type BaseDuration = Word8

-- | The LED intensity of the home light. It will always be limited to an interval
-- between 0 and 100.
type Intensity = Word8

-- | The fade duration factor of the home light. It will always be limited to an
-- interval between 0 and 15 and is multiplied with the 'BaseDuration' to obtain
-- the overall fade duration in milliseconds.
type FadeFactor = Word8

-- | The light duration factor of the home light. It will always be limited to an
-- interval between 0 and 15 and is multiplied with the 'BaseDuration' to obtain
-- the overall light duration in milliseconds.
type LightFactor = Word8

-- | A home light cycle consists of a target LED intensity, a fade factor which
-- controls the time needed to reach that LED intensity, and a light factor which
-- controls how long to keep the target LED intensity up.
type CycleConfig = (Intensity, FadeFactor, LightFactor)

-- | Defines the repeat behaviour after all the home light configuration cycles
-- have ended.
data RepeatBehaviour
  = Forever
    -- ^ Repeat the configured home light configuration cycles forever.
  | Times Word8
    -- ^ Repeat the configured home light configuration cycles a specific amount
    -- of times. It will always be limited to an interval between 1 and 15.
  deriving (Eq, Read, Show)

-- | The home light of a Nintendo Switch Controller can be controlled using repeatable
-- configuration cycles. See 'endlessPulse' for an example configuration.
data HomeLightConfig
  = Off
    -- ^ Turn off the home light.
  | Once BaseDuration Intensity CycleConfig
    -- ^ Given a start intensity of the home light LED, fade to a target LED
    -- intensity in a given time, and then keep this LED intensity up for a given
    -- amount of time.
    --
    -- The fade duration in milliseconds is calculated by multiplying the 'BaseDuration'
    -- with the 'FadeFactor' of the 'CycleConfig'. The light upkeep duration in
    -- milliseconds is calculated by multiplying the 'BaseDuration' with the 'LightFactor'
    -- of the 'CycleConfig'.
    --
    -- Example - fade from a switched off LED (@0@) to a fully bright LED (@100@) in 500ms
    -- (@50@ms * @10@), stay there for one second (@50@ms * @20@), then turn it off:
    --
    -- @
    --     'Once' 50 0 (100, 10, 20)
    -- @
  | Cyclic BaseDuration Intensity [CycleConfig] RepeatBehaviour
    -- ^ Given a start intensity of the home light LED, repeatedly fade to a
    -- target LED intensity in a given time, and then keep this LED intensity up
    -- for a given amount of time. The fade durations and light upkeep durations
    -- are calculated per cycle configuration as described for 'Once'. See
    -- 'endlessPulse' for a cyclic configuration example.
  deriving (Eq, Read, Show)

lightConfigCommand :: HomeLightConfig -> [Word8]
lightConfigCommand = \case
  Off ->
    replicate 25 0x00
  Once dur int (cycInt, fade, cycDur) ->
    let
      byte0  = scaleDuration dur
      byte1  = shiftL (scaleIntensity int) 4
      byte2  = shiftL (scaleIntensity cycInt) 4
      byte3H = clampMultiplier fade
      byte3L = clampMultiplier cycDur
    in
      byte0 : byte1 : byte2 : combine byte3H byte3L : replicate 45 0x00
  Cyclic dur int cfgs rep ->
    let
      cycles = take 15 cfgs
      byte0H = length cycles
      byte0L = scaleDuration dur
      byte1H = scaleIntensity int
      byte1L = case rep of
        Forever -> 0x0
        Times n -> clamp 1 15 n
      padded = cycles ++ replicate (16 - byte0H) (0,0,0)
      pairBytes = fmap (uncurry pairCommand) $ pairs padded
    in
      combine (fromIntegral byte0H) byte0L
        : combine byte1H byte1L
        : join pairBytes
       ++ replicate 23 0x00
  where
    scaleIntensity   = discretize 0 100 0 15
    scaleDuration    = discretize 8 175 0 15
    clampMultiplier  = clamp 0 15
    
    pairCommand :: CycleConfig -> CycleConfig -> [Word8]
    pairCommand (int1, fade1, dur1) (int2, fade2, dur2) =
      let
        byte0H = scaleIntensity int1
        byte0L = scaleIntensity int2
        byte1H = clampMultiplier fade1
        byte1L = clampMultiplier dur1
        byte2H = clampMultiplier fade2
        byte2L = clampMultiplier dur2
      in
        [combine byte0H byte0L, combine byte1H byte1L, combine byte2H byte2L]

-- | A convenient home light configuration which pulsates the home light LED:
--
-- @
--     'Cyclic'
--       ( 100 )         -- Base duration factor is 100ms.
--       (   0 )         -- LED is turned off at the beginning (intensity 0).
--       [ (100, 5, 1)   -- Fade to LED intensity 100 in 500ms (100ms * 5) and stay there for 100ms (100ms * 1).
--       , (  0, 5, 1) ] -- Fade to LED intensity   0 in 500ms (100ms * 5) and stay there for 100ms (100ms * 1).
--       ( Forever )     -- Repeat these two cycles forever, thus generating a pulse-like LED.
-- @
endlessPulse :: HomeLightConfig
endlessPulse =
  Cyclic
    ( 100 )
    ( 0   )
    [ (100, 5, 1), (0, 5, 1) ]
    ( Forever )

-- | Sets the home light (i.e., the LED ring around the home button) of a Nintendo
-- Switch controller.
--
-- Note: After sending a command like this to a controller, it is highly advised
-- to check its corresponding 'Device.Nintendo.Switch.CommandReply'
-- ('Device.Nintendo.Switch.SetHomeLight', to be exact) or at least call
-- 'Device.Nintendo.Switch.getInput' once before sending another command to
-- that controller. The function 'Device.Nintendo.Switch.withCommandReply' is a
-- convenient way to wait for a specific command reply from the controller.
setHomeLight :: HasHomeLight t => HomeLightConfig -> Controller t -> IO ()
setHomeLight cfg controller =
  sendSubcommand controller 0x01 0x38 $
    lightConfigCommand cfg

-- | Enables ('True') or disables ('False') the inertial measurement unit (i.e., 
-- accelerometer, gyroscope) of a Nintendo Switch controller. Inertial measurement
-- is disabled by default.
--
-- Note: After sending a command like this to a controller, it is highly advised
-- to check its corresponding 'Device.Nintendo.Switch.CommandReply'
-- ('Device.Nintendo.Switch.SetInertialMeasurement', to be exact) or at least call
-- 'Device.Nintendo.Switch.getInput' once before sending another command to
-- that controller. The function 'Device.Nintendo.Switch.withCommandReply' is a
-- convenient way to wait for a specific command reply from the controller.
setInertialMeasurement :: Bool -> Controller t -> IO ()
setInertialMeasurement on controller =
  sendSubcommand controller 0x01 0x40 $
    (if on then 0x01 else 0x00) : replicate 48 0x00

-- | The input mode of a Nintendo Switch controller determines the frequency and
-- amount of information received by 'Device.Nintendo.Switch.getInput'.
data InputMode
  = Standard
    -- ^ The default input mode. In this mode, controllers push 'Device.Nintendo.Switch.Input'
    -- packages in a 60Hz (Joy-Con) or 120Hz (Pro Controller) frequency, including
    -- 'Device.Nintendo.Switch.battery' information, 'Device.Nintendo.Switch.Analog' stick
    -- directions ('Device.Nintendo.Switch.stickLeft', 'Device.Nintendo.Switch.stickRight')
    -- and 'Device.Nintendo.Switch.Inertial' sensor data ('Device.Nintendo.Switch.extras')
    -- if activated via 'setInertialMeasurement'.
  | Simple
    -- ^ A simple input mode where a controller only pushes its 'Device.Nintendo.Switch.Input'
    -- whenever a button is pressed or a 'Device.Nintendo.Switch.CommandReply' ('Device.Nintendo.Switch.extras')
    -- is sent. In this mode, controllers only send 'Device.Nintendo.Switch.Discrete' stick
    -- directions ('Device.Nintendo.Switch.stickLeft', 'Device.Nintendo.Switch.stickRight')
    -- and no inertial sensor data. Furthermore, 'Device.Nintendo.Switch.battery' information
    -- is only sent in combination with command replies.
  deriving (Eq, Read, Show)

-- | Sets the input mode of a Nintendo Switch controller.
--
-- Note: After sending a command like this to a controller, it is highly advised
-- to check its corresponding 'Device.Nintendo.Switch.CommandReply'
-- ('Device.Nintendo.Switch.SetInputMode', to be exact) or at least call
-- 'Device.Nintendo.Switch.getInput' once before sending another command to
-- that controller. The function 'Device.Nintendo.Switch.withCommandReply' is a
-- convenient way to wait for a specific command reply from the controller.
setInputMode :: HasInputMode t => InputMode -> Controller t -> IO ()
setInputMode = setInputModeInternal

setInputModeInternal :: InputMode -> Controller t -> IO ()
setInputModeInternal mode controller =
  sendSubcommand controller 0x01 0x03 $
    toByte mode : replicate 48 0x00
  where
    toByte = \case
      Standard -> 0x30
      Simple   -> 0x3F

neutralPartRumble :: [Word8]
neutralPartRumble = [0x00, 0x01, 0x40, 0x40]

neutralRumble :: [Word8]
neutralRumble = neutralPartRumble ++ neutralPartRumble

sendCommand :: Device -> Word8 -> [Word8] -> IO ()
sendCommand dev cmdID cmdData = do
  size <- write dev $ BS.pack (cmdID : cmdData)
  when (size <= 0) $ throwIO WriteException -- should check the size more precisely here

sendRawSubcommand :: Device -> IORef Word8 -> Word8 -> Word8 -> [Word8] -> IO ()
sendRawSubcommand dev ref cmdID subID subData = do
  count <- readIORef ref
  sendCommand dev cmdID $ count : neutralRumble ++ subID : subData
  writeIORef ref $ 0x0F .&. succ count

sendSubcommand :: Controller t -> Word8 -> Word8 -> [Word8] -> IO ()
sendSubcommand controller =
  sendRawSubcommand
    ( handle controller )
    ( counter controller )

encodeHF :: (Floating a, RealFrac a) => a -> (Word8, Word8)
encodeHF freq = (hfH, hfL)
  where clamped = clamp 81.75177 1252.572266 freq
        base = round $ logBase 2 (clamped * 0.1) * 32
        hf = (base - 0x60) * 4 :: Int
        hfH = fromIntegral $ hf .&. 0xFF
        hfL = fromIntegral $ shiftR hf 8 .&. 0xFF

encodeLF :: (Floating a, RealFrac a) => a -> Word8
encodeLF freq = base - 0x40
  where clamped = clamp 40.875885 626.286133 freq
        base = round $ logBase 2 (clamped * 0.1) * 32

encodeHA :: (Floating a, RealFrac a) => a -> Word8
encodeHA amp | clamped < 0.117 = round $ (base - 0x60) / (5 - (2 ** clamped)) - 1
             | clamped >= 0.23 = round $ (base - 0x60) * 2 - 0xF6
             | otherwise = round $ base - 0xBC
  where clamped = clamp 0.0 1.0 amp
        base = logBase 2 (clamped * 1000) * 32

encodeLA :: (Floating a, RealFrac a) => a -> (Word8, Word8)
encodeLA amp = (laH, laL)
  where encoded = encodeHA amp `div` 2
        isOdd = odd encoded
        laH = if isOdd then 0x80 else 0x00
        laL = 0x40 + (if isOdd then encoded - 1 else encoded) `div` 2

rumblePartCommand :: RumbleConfig -> [Word8]
rumblePartCommand cfg = let
    (hfH, hfL) = encodeHF $ highFrequency cfg
    ha         = encodeHA $ highAmplitude cfg
    lf         = encodeLF $ lowFrequency cfg
    (laH, laL) = encodeLA $ lowAmplitude cfg
  in [hfH, ha + hfL, lf + laH, laL]

-- | Nintendo Switch controllers have a HD rumble feature which allows
-- fine-grained control of rumble strengths and directions. As a consequence,
-- a rumble is not configured by a mere numeric value, but by two (high and low)
-- pairs of frequencies and amplitudes. This library constrains the value ranges
-- of frequencies and amplitudes in order to always obtain sane configurations.
-- However, sending extreme values for these pairs over an extended period of time
-- may still damage a controller, so experiment wisely with rather short rumbles.
--
-- For technical discussions and the meaning of these values, one can read
-- <https://github.com/dekuNukem/Nintendo_Switch_Reverse_Engineering/issues/11 this>,
-- for example. A sample rumble configuration is provided by 'normalRumble'.
data RumbleConfig =
  RumbleConfig
    { highFrequency :: Double
      -- ^ The high frequency. It will always be limited to an interval between
      -- 81.75177 Hz and 1252.572266 Hz.
    , highAmplitude :: Double
      -- ^ The high amplitude. It will always be limited to an interval between
      -- 0.0 and 1.0.
    , lowFrequency  :: Double
      -- ^ The low frequency. It will always be limited to an interval between
      -- 40.875885 Hz and 626.286133 Hz.
    , lowAmplitude  :: Double
      -- ^ The low amplitude. It will always be limited to an interval between
      -- 0.0 and 1.0.
    }
  deriving (Eq, Read, Show)

-- | A convenient rumble configuration indicating a medium rumble strength.
--
-- @
--     'RumbleConfig'
--       { 'highFrequency' = 800
--       , 'highAmplitude' = 0.5
--       , 'lowFrequency'  = 330
--       , 'lowAmplitude'  = 0.75
--       }
-- @
normalRumble :: RumbleConfig
normalRumble =
  RumbleConfig
    { highFrequency = 800
    , highAmplitude = 0.5
    , lowFrequency  = 330
    , lowAmplitude  = 0.75
    }

-- | A convenient rumble configuration indicating no rumble.
noRumble :: RumbleConfig
noRumble =
  RumbleConfig
    { highFrequency = 320
    , highAmplitude = 0
    , lowFrequency  = 160
    , lowAmplitude  = 0
    }

-- | Enables ('True') or disables ('False') the rumble feature of a Nintendo
-- Switch controller. The rumble feature is disabled by default.
--
-- Note: After sending a command like this to a controller, it is highly advised
-- to check its corresponding 'Device.Nintendo.Switch.CommandReply'
-- ('Device.Nintendo.Switch.SetVibration', to be exact) or at least call
-- 'Device.Nintendo.Switch.getInput' once before sending another command to
-- that controller. The function 'Device.Nintendo.Switch.withCommandReply' is a
-- convenient way to wait for a specific command reply from the controller.
setVibration :: Bool -> Controller t -> IO ()
setVibration on controller =
  sendSubcommand controller 0x01 0x48 $
    (if on then 0x01 else 0x00) : replicate 48 0x00

-- | Sets the left rumble of a Nintendo Switch controller.
setLeftRumble :: HasLeftRumble t => RumbleConfig -> Controller t -> IO ()
setLeftRumble cfg controller =
  sendRawRumbleCommand
    ( handle controller )
    ( rumblePartCommand cfg )
    ( neutralPartRumble )

-- | Sets the right rumble of a Nintendo Switch controller.
setRightRumble :: HasRightRumble t => RumbleConfig -> Controller t -> IO ()
setRightRumble cfg controller =
  sendRawRumbleCommand
    ( handle controller )
    ( neutralPartRumble )
    ( rumblePartCommand cfg )

-- | Sets both the left rumble and right rumble of a Nintendo Switch controller.
-- Note that this is more efficient than setting the left rumble and right rumble
-- separately via 'setLeftRumble' and 'setRightRumble'.
setRumble
  :: (HasLeftRumble t, HasRightRumble t)
  => RumbleConfig -- ^ The left-side rumble configuration.
  -> RumbleConfig -- ^ The right-side rumble configuration.
  -> Controller t -- ^ The controller which should rumble.
  -> IO ()
setRumble leftCfg rightCfg controller =
  sendRawRumbleCommand
    ( handle controller )
    ( rumblePartCommand leftCfg )
    ( rumblePartCommand rightCfg )

sendRawRumbleCommand :: Device -> [Word8] -> [Word8] -> IO ()
sendRawRumbleCommand dev left right =
  sendCommand dev 0x10 $ 0x00 : left ++ right ++ replicate 40 0x00

requestRawSPI :: Device -> IORef Word8 -> Word32 -> Word8 -> IO ()
requestRawSPI dev ref start len =
  let
    byte0 = fromIntegral $         start .&. 0x000000FF
    byte1 = fromIntegral $ shiftR (start .&. 0x0000FF00) 8
    byte2 = fromIntegral $ shiftR (start .&. 0x00FF0000) 16
    byte3 = fromIntegral $ shiftR (start .&. 0xFF000000) 24
  in
  sendRawSubcommand dev ref 0x01 0x10 $
    [byte0, byte1, byte2, byte3, clamp 0x00 0x1D len] ++ replicate 44 0x00

-- | Nintendo Switch controllers have four LEDs that can be used to indicate
-- various things, for example the player number or the Bluetooth pairing status.
-- The LEDs are numbered from left to right (i.e., 'led0' is the leftmost LED,
-- 'led3' is the rightmost LED).
data PlayerLightsConfig =
  PlayerLightsConfig
    { led0 :: LightMode
    , led1 :: LightMode
    , led2 :: LightMode
    , led3 :: LightMode
    }
  deriving (Eq, Read, Show)

-- | Each player light LED can be individually turned on, turned off or used in
-- a pulsating manner (i.e., flashing).
data LightMode
  = LightOn
  | LightOff
  | Flashing
  deriving (Eq, Read, Show)

-- | A convenient player lights configuration where all LEDs are turned off.
noPlayerLights :: PlayerLightsConfig
noPlayerLights =
  PlayerLightsConfig
    { led0 = LightOff
    , led1 = LightOff
    , led2 = LightOff
    , led3 = LightOff
    }

-- | A convenient player lights configuration indicating player one (i.e., 'led0' is set).
playerOne :: PlayerLightsConfig
playerOne = noPlayerLights { led0 = LightOn }

-- | A convenient player lights configuration indicating player two (i.e., 'led1' is set).
playerTwo :: PlayerLightsConfig
playerTwo = noPlayerLights { led1 = LightOn }

-- | A convenient player lights configuration indicating player three (i.e., 'led2' is set).
playerThree :: PlayerLightsConfig
playerThree = noPlayerLights { led2 = LightOn }

-- | A convenient player lights configuration indicating player four (i.e., 'led3' is set).
playerFour :: PlayerLightsConfig
playerFour = noPlayerLights { led3 = LightOn }

-- | A convenient player lights configuration where all LEDs are flashing.
flashAll :: PlayerLightsConfig
flashAll =
  PlayerLightsConfig
    { led0 = Flashing
    , led1 = Flashing
    , led2 = Flashing
    , led3 = Flashing
    }

-- | Sets the player lights of a Nintendo Switch controller.
--
-- Note: After sending a command like this to a controller, it is highly advised
-- to check its corresponding 'Device.Nintendo.Switch.CommandReply'
-- ('Device.Nintendo.Switch.SetPlayerLights', to be exact) or at least call
-- 'Device.Nintendo.Switch.getInput' once before sending another command to
-- that controller. The function 'Device.Nintendo.Switch.withCommandReply' is a
-- convenient way to wait for a specific command reply from the controller.
setPlayerLights :: HasPlayerLights t => PlayerLightsConfig -> Controller t -> IO ()
setPlayerLights config controller =
  sendSubcommand controller 0x01 0x30 $
    [ setBit led0 0x01
    . setBit led1 0x02
    . setBit led2 0x04
    . setBit led3 0x08
    $ 0x00
    ]
  where
    setBit f position =
      case f config of
        LightOn  -> (position .|.)
        LightOff -> id
        Flashing -> (shiftL position 4 .|.)