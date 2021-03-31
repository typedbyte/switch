{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Main where

-- base
import Control.Monad (forM_, void)

-- switch
import Device.Nintendo.Switch

import ControllerTest (ackInputMode, rightTest, sensorTest)

main :: IO ()
main = do
  putStrLn "----------------------------------------------------------"
  putStrLn "Testing all connected right Joy-Cons one after another ..."
  putStrLn "Press Y    to enable the home light."
  putStrLn "Press X    to disable the home light."
  putStrLn "Press B    to enable controller vibration."
  putStrLn "Press A    to disable controller vibration."
  putStrLn "Press +    to test the sensors and analog stick input."
  putStrLn "Press Home to end the test of the sensors and the stick."
  putStrLn "----------------------------------------------------------"
  withConsole $ \switch -> do
    infos <- getControllerInfos @'RightJoyCon switch
    forM_ infos $ \info ->
      withController info $ \controller -> do
        setInputMode Simple controller
        ackInputMode controller -- ack
        setPlayerLights flashAll controller
        void $ getInput controller -- ack
        setVibration True controller
        rightTest controller -- ack comes with first input
        setInertialMeasurement True controller
        void $ getInput controller -- ack
        setInputMode Standard controller
        sensorTest btnHome controller -- ack comes with first input
        setInputMode Simple controller
        void $ getInput controller -- ack