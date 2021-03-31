{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Main where

-- base
import Control.Monad (forM_, void)

-- switch
import Device.Nintendo.Switch

import ControllerTest (ackInputMode, leftTest, sensorTest)

main :: IO ()
main = do
  putStrLn "------------------------------------------------------------------"
  putStrLn "Testing all connected left Joy-Cons one after another ..."
  putStrLn "Press Arrow Down  to enable controller vibration."
  putStrLn "Press Arrow Right to disable controller vibration."
  putStrLn "Press -           to test the sensors and analog stick input."
  putStrLn "Press Capture     to end the test of the sensors and the stick."
  putStrLn "------------------------------------------------------------------"
  withConsole $ \switch -> do
    infos <- getControllerInfos @'LeftJoyCon switch
    forM_ infos $ \info ->
      withController info $ \controller -> do
        setInputMode Simple controller
        ackInputMode controller -- ack
        setPlayerLights flashAll controller
        void $ getInput controller -- ack
        setVibration True controller
        leftTest controller -- ack comes with first input
        setInertialMeasurement True controller
        void $ getInput controller -- ack
        setInputMode Standard controller
        sensorTest btnCapture controller -- ack comes with first input
        setInputMode Simple controller
        void $ getInput controller -- ack