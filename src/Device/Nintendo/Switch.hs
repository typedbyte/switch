-----------------------------------------------------------------------------
-- |
-- Module      :  Device.Nintendo.Switch
-- Copyright   :  (c) Michael Szvetits, 2021
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- Types and functions for connecting to Nintendo Switch controllers, reading
-- input (e.g., buttons, sensors) and sending commands (e.g., rumble).
-----------------------------------------------------------------------------
module Device.Nintendo.Switch
  ( -- * Connection
    -- ** Switch Console
    Console
  , init
  , exit
  , withConsole
    -- ** Switch Controllers
  , ControllerType(..)
  , ControllerInfo
  , Controller
  , getControllerInfos
  , connect
  , disconnect
  , withController
    -- * Controller Input
    -- ** Input Mode
  , InputMode(..)
  , setInputMode
  , setInertialMeasurement
    -- ** Getting Input
  , getInput
  , getTimeoutInput
    -- ** Input Types
  , Input
  , ControllerInput(..)
  , StickDirection(..)
  , Direction(..)
  , BatteryInfo(..)
  , BatteryStatus(..)
  , ExtraInput(..)
  , Accelerometer
  , Gyroscope
  , ReplyData(SetHomeLight, SetInertialMeasurement, SetInputMode, SetPlayerLights, SetVibration)
  , Acknowledgement(..)
    -- ** Convenience
  , noInput
  , coordinates
  , mergeInputs
  , withCommandReply
    -- * Controller Output
    -- ** Home Light
  , setHomeLight
  , HomeLightConfig(..)
  , CycleConfig
  , BaseDuration
  , Intensity
  , FadeFactor
  , LightFactor
  , RepeatBehaviour
  , endlessPulse
    -- ** Player Lights
  , setPlayerLights
  , PlayerLightsConfig(..)
  , LightMode(..)
  , noPlayerLights
  , playerOne
  , playerTwo
  , playerThree
  , playerFour
  , flashAll
    -- ** Rumble
  , setVibration
  , setLeftRumble
  , setRightRumble
  , setRumble
  , RumbleConfig(..)
  , normalRumble
  , noRumble
    -- * Exceptions
  , ConnectionException(..)
  , InputException(..)
  , OutputException(..)
    -- * Type Classes
  , IsController
  , HasCalibration
  , HasInputMode
  , HasInput
  , HasHomeLight
  , HasPlayerLights
  , HasLeftRumble
  , HasRightRumble
  ) where

-- base
import Prelude hiding (init)

-- switch
import Device.Nintendo.Switch.Connection
import Device.Nintendo.Switch.Controller
import Device.Nintendo.Switch.Input
import Device.Nintendo.Switch.Output