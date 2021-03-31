{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Main where

-- base
import Control.Monad (forM_, void)

-- switch
import Device.Nintendo.Switch

import ControllerTest (leftTest, rightTest, sensorTest)

main :: IO ()
main = do
  putStrLn "------------------------------------------------------------------"
  putStrLn "Testing all connected Pro controllers one after another ..."
  putStrLn "------------------------------------------------------------------"
  withConsole $ \switch -> do
    infos <- getControllerInfos @'ProController switch
    forM_ infos $ \info ->
      withController info $ \controller -> do
        setPlayerLights flashAll controller
        void $ getInput controller -- ack
        setInertialMeasurement False controller
        void $ getInput controller -- ack
        setVibration True controller
        putStrLn "------------------------------------------------------------------"
        putStrLn "Testing left side of Pro controller ..."
        putStrLn "Press Arrow Down  to enable controller vibration."
        putStrLn "Press Arrow Right to disable controller vibration."
        putStrLn "Press -           to switch to right side."
        putStrLn "------------------------------------------------------------------"
        leftTest controller -- ack comes with first input
        putStrLn "------------------------------------------------------------------"
        putStrLn "Testing right side of Pro controller ..."
        putStrLn "Press Y    to enable the home light."
        putStrLn "Press X    to disable the home light."
        putStrLn "Press B    to enable controller vibration."
        putStrLn "Press A    to disable controller vibration."
        putStrLn "Press +    to test the sensors and analog stick inputs."
        putStrLn "Press Home to end the test of the sensors and the sticks."
        putStrLn "------------------------------------------------------------------"
        rightTest controller
        setInertialMeasurement True controller
        sensorTest btnHome controller -- ack comes with first input