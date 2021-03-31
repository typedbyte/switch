{-# LANGUAGE DataKinds #-}
module Main where

-- base
import Control.Monad          (forM_, unless, void)
import Control.Monad.IO.Class (liftIO)

-- GPipe
import Graphics.GPipe

-- GPipe-GLFW
import qualified Graphics.GPipe.Context.GLFW as GLFW

-- switch
import Device.Nintendo.Switch

import Madgwick

toV3 :: V4 a -> V3 a
toV3 (V4 x y z _) = V3 x y z

toV4 :: Num a => V3 a -> V4 a
toV4 (V3 x y z) = V4 x y z 1

rot :: Num a => Quaternion a -> V3 a -> V3 a
rot (Quaternion s u) v = (V3 a a a) * u + (V3 b b b) * v + (V3 c c c) * cross u v
  where a = 2 * (dot u v)
        b = s*s - dot u u
        c = 2 * s

main :: IO ()
main = do
  putStrLn "--------------------------------------------------------------------"
  putStrLn "Testing sensors of all connected left Joy-Cons one after another ..."
  putStrLn "Close the window to end the test run."
  putStrLn "--------------------------------------------------------------------"
  withConsole $ \switch -> do
    infos <- getControllerInfos switch
    forM_ infos $ \info ->
      withController info $ \controller -> do
        setInertialMeasurement True controller
        void $ getInput controller -- ack
        runContextT GLFW.defaultHandleConfig $ do
          -- window
          let windowConfig =
                GLFW.WindowConfig
                  800 800
                  "Left Joy-Con 3D Test (will drift, press Capture to reset)"
                  Nothing [] Nothing
          win <- newWindow (WindowFormatColor RGB8) windowConfig
          
          -- cube vertices
          vertexBuffer <- newBuffer 36
          writeBuffer vertexBuffer 0
            [ (V4 (-0.5) (-0.5) (-0.5) 1, V3 1 0 1)
            , (V4 ( 0.5) ( 0.5) (-0.5) 1, V3 1 0 1)
            , (V4 ( 0.5) (-0.5) (-0.5) 1, V3 1 0 1)
            , (V4 (-0.5) (-0.5) (-0.5) 1, V3 1 0 1)
            , (V4 (-0.5) ( 0.5) (-0.5) 1, V3 1 0 1)
            , (V4 ( 0.5) ( 0.5) (-0.5) 1, V3 1 0 1)
            
            , (V4 (-0.5) (-0.5) (-0.5) 1, V3 0 0 1)
            , (V4 (-0.5) (-0.5) ( 0.5) 1, V3 0 0 1)
            , (V4 (-0.5) ( 0.5) ( 0.5) 1, V3 0 0 1)
            , (V4 (-0.5) (-0.5) (-0.5) 1, V3 0 0 1)
            , (V4 (-0.5) ( 0.5) ( 0.5) 1, V3 0 0 1)
            , (V4 (-0.5) ( 0.5) (-0.5) 1, V3 0 0 1)
            
            , (V4 ( 0.5) (-0.5) (-0.5) 1, V3 0 1 0)
            , (V4 ( 0.5) ( 0.5) ( 0.5) 1, V3 0 1 0)
            , (V4 ( 0.5) (-0.5) ( 0.5) 1, V3 0 1 0)
            , (V4 ( 0.5) (-0.5) (-0.5) 1, V3 0 1 0)
            , (V4 ( 0.5) ( 0.5) (-0.5) 1, V3 0 1 0)
            , (V4 ( 0.5) ( 0.5) ( 0.5) 1, V3 0 1 0)
            
            , (V4 (-0.5) (-0.5) ( 0.5) 1, V3 1 0 0)
            , (V4 ( 0.5) (-0.5) ( 0.5) 1, V3 1 0 0)
            , (V4 ( 0.5) ( 0.5) ( 0.5) 1, V3 1 0 0)
            , (V4 (-0.5) (-0.5) ( 0.5) 1, V3 1 0 0)
            , (V4 ( 0.5) ( 0.5) ( 0.5) 1, V3 1 0 0)
            , (V4 (-0.5) ( 0.5) ( 0.5) 1, V3 1 0 0)
            
            , (V4 (-0.5) (-0.5) (-0.5) 1, V3 0 1 1)
            , (V4 ( 0.5) (-0.5) (-0.5) 1, V3 0 1 1)
            , (V4 ( 0.5) (-0.5) ( 0.5) 1, V3 0 1 1)
            , (V4 (-0.5) (-0.5) (-0.5) 1, V3 0 1 1)
            , (V4 ( 0.5) (-0.5) ( 0.5) 1, V3 0 1 1)
            , (V4 (-0.5) (-0.5) ( 0.5) 1, V3 0 1 1)
            
            , (V4 (-0.5) ( 0.5) (-0.5) 1, V3 1 1 0)
            , (V4 ( 0.5) ( 0.5) ( 0.5) 1, V3 1 1 0)
            , (V4 ( 0.5) ( 0.5) (-0.5) 1, V3 1 1 0)
            , (V4 (-0.5) ( 0.5) (-0.5) 1, V3 1 1 0)
            , (V4 (-0.5) ( 0.5) ( 0.5) 1, V3 1 1 0)
            , (V4 ( 0.5) ( 0.5) ( 0.5) 1, V3 1 1 0)
            ]
          
          uniformBuffer <- newBuffer 1
          
          shader <- compileShader $ do
            quart <- getUniform (const (uniformBuffer,0))
            primitiveStream <- toPrimitiveStream id
            let rotatedStream = fmap (\(v,c) -> (toV4 (rot quart (toV3 v)), c)) primitiveStream
            fragmentStream <- rasterize (const (Front, ViewPort (V2 0 0) (V2 800 800), DepthRange 0 1)) rotatedStream
            drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) fragmentStream
          
          loop controller (Quaternion 1 (V3 0 0 0)) vertexBuffer uniformBuffer shader win
        setInputMode Simple controller
        void $ getInput controller -- ack

loop
  :: Controller 'LeftJoyCon
  -> Quaternion Float
  -> Buffer os (B4 Float, B3 Float)
  -> Buffer os (Uniform (Quaternion (B Float)))
  -> CompiledShader os (PrimitiveArray Triangles (B4 Float, B3 Float))
  -> Window os RGBFloat ()
  -> ContextT GLFW.Handle os IO ()
loop controller quart vertexBuffer uniformBuffer shader win = do
  input <- liftIO $ getInput controller
  let newQuart@(Quaternion w (V3 xq yq zq)) =
        -- Capture button resets rotation because of sensor drift
        if btnCapture input then
          Quaternion 1 (V3 0 0 0)
        else
          case extras input of
            Inertial (a1,a2,a3) (g1, g2, g3) ->
              let
                -- convert Joy-Con axes to Madgwick axes
                flipAxes (x,y,z) = (x,-y,-z)
                -- disable Z rotation (yaw) to stabilize output with 6 DoF IMU
                zeroZ (x,y,_) = (x,y,0)
                -- beta parameter can be tweaked for Madgwick algorithm
                beta = 0.01
                -- Joy-Con provides three measurements in 15ms, hence a 5ms rate
                freq = 1 / 0.005
                -- Madgwick algorithm for accelerometer and gyroscope sensor fusion
                newQ1 = madgwick (flipAxes a1) (flipAxes $ zeroZ g1) beta freq quart
                newQ2 = madgwick (flipAxes a2) (flipAxes $ zeroZ g2) beta freq newQ1
                newQ3 = madgwick (flipAxes a3) (flipAxes $ zeroZ g3) beta freq newQ2
              in newQ3
            _ -> quart
  -- convert Madgwick axes to OpenGL axes
  let openGLquart = Quaternion w (V3 (-yq) zq xq)
  writeBuffer uniformBuffer 0 [openGLquart]
  render $ do
    clearWindowColor win (V3 0 0 0)
    vertexArray <- newVertexArray vertexBuffer
    shader $ toPrimitiveArray TriangleList vertexArray
  swapWindowBuffers win

  closeRequested <- GLFW.windowShouldClose win
  unless (closeRequested == Just True) $
    loop controller newQuart vertexBuffer uniformBuffer shader win