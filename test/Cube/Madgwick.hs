module Madgwick where

-- linear
import Linear.Quaternion
import Linear.V3

-- Madgwick's sensor fusion algorithm.
-- See: https://x-io.co.uk/open-source-imu-and-ahrs-algorithms/
madgwick
  :: (Float, Float, Float)
  -> (Float, Float, Float)
  -> Float
  -> Float
  -> Quaternion Float
  -> Quaternion Float
madgwick (ax,ay,az) (gx,gy,gz) beta sampleFreq (Quaternion q0 (V3 q1 q2 q3)) =
  let
    qDot1 = 0.5 * (-q1 * gx - q2 * gy - q3 * gz)
    qDot2 = 0.5 * (q0 * gx + q2 * gz - q3 * gy)
    qDot3 = 0.5 * (q0 * gy - q1 * gz + q3 * gx)
    qDot4 = 0.5 * (q0 * gz + q1 * gy - q2 * gx)

    (newDot1, newDot2, newDot3, newDot4) = help qDot1 qDot2 qDot3 qDot4

    q0t = q0 + newDot1 * (1.0 / sampleFreq)
    q1t = q1 + newDot2 * (1.0 / sampleFreq)
    q2t = q2 + newDot3 * (1.0 / sampleFreq)
    q3t = q3 + newDot4 * (1.0 / sampleFreq)

    recipNorm = 1 / sqrt (q0t * q0t + q1t * q1t + q2t * q2t + q3t * q3t)
  in
    Quaternion (q0t * recipNorm) (V3 (q1t * recipNorm) (q2t * recipNorm) (q3t * recipNorm))
  where
    help qDot1 qDot2 qDot3 qDot4 =
      if(not ((ax == 0) && (ay == 0) && (az == 0))) then
        let recipNorm = 1 / sqrt (ax * ax + ay * ay + az * az)
            fax = ax * recipNorm
            fay = ay * recipNorm
            faz = az * recipNorm;  

            _2q0 = 2.0 * q0
            _2q1 = 2.0 * q1
            _2q2 = 2.0 * q2
            _2q3 = 2.0 * q3
            _4q0 = 4.0 * q0
            _4q1 = 4.0 * q1
            _4q2 = 4.0 * q2
            _8q1 = 8.0 * q1
            _8q2 = 8.0 * q2
            q0q0 = q0 * q0
            q1q1 = q1 * q1
            q2q2 = q2 * q2
            q3q3 = q3 * q3

            s0 = _4q0 * q2q2 + _2q2 * fax + _4q0 * q1q1 - _2q1 * fay
            s1 = _4q1 * q3q3 - _2q3 * fax + 4.0 * q0q0 * q1 - _2q0 * fay - _4q1 + _8q1 * q1q1 + _8q1 * q2q2 + _4q1 * faz
            s2 = 4.0 * q0q0 * q2 + _2q0 * fax + _4q2 * q3q3 - _2q3 * fay - _4q2 + _8q2 * q1q1 + _8q2 * q2q2 + _4q2 * faz
            s3 = 4.0 * q1q1 * q3 - _2q1 * fax + 4.0 * q2q2 * q3 - _2q2 * fay
            recipNorm2 = 1 / sqrt (s0 * s0 + s1 * s1 + s2 * s2 + s3 * s3)
        in (qDot1 - beta * s0 * recipNorm2, qDot2 - beta * s1 * recipNorm2, qDot3 - beta * s2 * recipNorm2, qDot4 - beta * s3 * recipNorm2)
      else
        (qDot1, qDot2, qDot3, qDot4)