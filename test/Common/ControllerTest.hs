{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module ControllerTest where

-- base
import Control.Monad (unless)
import Text.Printf   (printf)

-- switch
import Device.Nintendo.Switch

ackInputMode :: Controller t -> IO ()
ackInputMode controller =
  withCommandReply 10 50 controller $ \case
    SetInputMode (ACK ()) -> Just ()
    _                     -> Nothing

leftTest :: (HasInput t, HasLeftRumble t) => Controller t -> IO ()
leftTest controller = loop noInput
  where
    loop old = do
      input <- getInput controller
      if | old == input   -> loop input -- to prevent flooding the output, only show changes
         | btnMinus input -> pure ()    -- end on - button
         | otherwise -> do
             putStrLn $ show input
             if | btnDown input  -> setLeftRumble normalRumble controller
                | btnRight input -> setLeftRumble noRumble controller
                | otherwise      -> pure ()
             loop input

rightTest :: (HasInput t, HasRightRumble t, HasHomeLight t) => Controller t -> IO ()
rightTest controller = loop noInput
  where
    loop old = do
      input <- getInput controller
      if | old == input  -> loop input -- to prevent flooding the output, only show changes
         | btnPlus input -> pure ()    -- end on + button
         | otherwise -> do
             putStrLn $ show input
             if | btnY input -> setHomeLight endlessPulse controller
                | btnX input -> setHomeLight Off controller
                | btnB input -> setRightRumble normalRumble controller
                | btnA input -> setRightRumble noRumble controller
                | otherwise  -> pure ()
             loop input

sensorTest :: HasInput t => (Input -> Bool) -> Controller t -> IO ()
sensorTest end controller = do
  input <- getInput controller
  case extras input of
    -- for simplicity, we only take one of three measurements.
    Inertial (acc,_,_) (gyro,_,_) ->
      let
        (ax, ay, az) = acc
        (gx, gy, gz) = gyro
        (lx, ly) = coordinates $ stickLeft input
        (rx, ry) = coordinates $ stickRight input
      in
        putStrLn $
          printf
            (  "Left Stick: %+.2f / %+.2f | "
            ++ "Right Stick: %+.2f / %+.2f | "
            ++ "Accelerometer (in Gs): %+.2f / %+.2f / %+.2f | "
            ++ "Gyroscope (in radians/s): %+.2f / %+.2f / %+.2f" )
          lx ly rx ry ax ay az gx gy gz
    _ -> pure ()
  unless (end input) (sensorTest end controller)