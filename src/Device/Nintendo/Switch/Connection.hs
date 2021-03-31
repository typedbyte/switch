{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Device.Nintendo.Switch.Connection where

-- attoparsec
import Data.Attoparsec.ByteString (Parser, maybeResult, parse)

-- base
import Control.Exception (Exception, bracket, throwIO)
import Data.IORef        (newIORef)
import Prelude    hiding (init)

-- bytestring
import Data.ByteString (ByteString)

-- switch
import Device.Nintendo.Switch.Controller (Controller(..), ControllerType(..),
                                          HasCalibration(..), RawCalibration(..),
                                          axisCalibrationParser, stickFactoryParser,
                                          parseStickUserCalibration, axisUserParser)
import Device.Nintendo.Switch.Input      (Acknowledgement(ACK), ReplyData(SetInputMode),
                                          withCommandReply, withRawSPIData)
import Device.Nintendo.Switch.Output     (InputMode(Standard), setInputModeInternal)

import qualified System.HIDAPI as HID

-- | A handle which represents a virtual Nintendo Switch console. The handle
-- is used to detect controllers and manage their connections.
data Console = Console
  deriving (Eq, Ord, Read, Show)

-- | Initializes a Nintendo Switch console handle. In other words, it lets us
-- pretend to be Nintendo Switch console in order to detect controllers and manage
-- their connections. You must call this first before doing anything else.
init :: IO Console
init = HID.init >> pure Console

-- | Destroys a virtual Nintendo Switch handle. You must call this when you are
-- finished talking to the controllers.
exit :: Console -> IO ()
exit Console = HID.exit

-- | A convenient wrapper around 'init' and 'exit'.
withConsole :: (Console -> IO a) -> IO a
withConsole = bracket init exit

-- | A handle which represents an unconnected Nintendo Switch controller.
newtype ControllerInfo (t :: ControllerType) =
  ControllerInfo HID.DeviceInfo
    deriving Show

-- | A constraint which indicates that a controller is a valid Nintendo Switch
-- controller that can be detected and connected to.
class IsController (t :: ControllerType) where
  productID :: HID.ProductID
  vendorID  :: HID.VendorID
  vendorID = 0x057E -- standard: Nintendo

instance IsController 'LeftJoyCon where
  productID = 0x2006

instance IsController 'RightJoyCon where
  productID = 0x2007

instance IsController 'ProController where
  productID = 0x2009

-- | A 'ConnectionException' is thrown if something goes wrong when reading the
-- internal data of a Nintendo Switch controller when connecting to it. This
-- should not occur if you have an unmodified controller (i.e., you have not
-- tampered with its internal SPI flash memory).
data ConnectionException
  = NoFactoryStickException -- ^ Indicates that a controller has no factory stick calibration.
  | NoFactoryAxisException  -- ^ Indicates that a controller has no factory sensor calibration.
  deriving Eq

instance Exception ConnectionException
instance Show ConnectionException where
  show NoFactoryStickException = "Could not determine the factory stick calibration."
  show NoFactoryAxisException  = "Could not determine the factory sensor calibration."

-- | Connects to a detected Nintendo Switch controller.
--
-- Can throw a 'ConnectionException' if something is very wrong with your
-- internal controller memory (i.e., if you have tampered with it).
connect
  :: forall t . HasCalibration t
  => ControllerInfo t  -- ^ The detected Nintendo Switch controller.
  -> IO (Controller t) -- ^ The connected Nintendo Switch controller.
connect (ControllerInfo devInfo) = do
  dev <- HID.openDeviceInfo devInfo
  ref <- newIORef 0
  facStick  <- withRawSPIData dev ref 0x603D 18 (tryParse stickFactoryParser)
  facAxis   <- withRawSPIData dev ref 0x6020 24 (tryParse axisCalibrationParser)
  (usl,usr) <- withRawSPIData dev ref 0x8010 22 parseStickUserCalibration
  usrAxis   <- withRawSPIData dev ref 0x8026 26 (tryParse axisUserParser)
  controller <-
    case (facStick, facAxis) of
      (Nothing, _) -> throwIO NoFactoryStickException
      (_, Nothing) -> throwIO NoFactoryAxisException
      (Just (fsl,fsr), Just sensor) ->
        let ls = maybe fsl id usl
            rs = maybe fsr id usr
            ax = maybe sensor id usrAxis 
            cal = calibrate @t (RawCalibration ls rs ax) in
        pure $ Controller dev ref cal
  setInputModeInternal Standard controller
  withCommandReply 10 50 controller $ \case
    SetInputMode (ACK ()) -> Just ()
    _                     -> Nothing
  pure controller
    where
      tryParse :: Parser a -> ByteString -> Maybe a
      tryParse parser = maybeResult . parse parser

-- | Disconnects a Nintendo Switch controller. You must not use the controller
-- handle after disconnecting.
disconnect :: Controller t -> IO ()
disconnect = HID.close . handle

-- | A convenient wrapper around 'connect' and 'disconnect'.
withController :: HasCalibration t => ControllerInfo t -> (Controller t -> IO a) -> IO a
withController info =
  bracket
    ( connect info )
    ( disconnect )

-- | Detects all Nintendo Switch controllers of a specific 'ControllerType',
-- usually connected via Bluetooth.
--
-- You may want to use this function with @TypeApplications@ if the controller
-- type cannot be inferred, like:
--
-- @
--     'getControllerInfos' \@''LeftJoyCon' console
-- @
-- 
getControllerInfos :: forall t . IsController t => Console -> IO [ControllerInfo t]
getControllerInfos Console =
  let
    vendID = vendorID @t
    prodID = productID @t
  in do
    devInfos <- HID.enumerate (Just vendID) (Just prodID)
    pure $ ControllerInfo <$> devInfos