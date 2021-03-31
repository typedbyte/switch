module Device.Nintendo.Switch.Utils where

-- attoparsec
import Data.Attoparsec.ByteString (Parser, anyWord8)

-- base
import Data.Bits ((.|.), (.&.), Bits, shiftL)
import Data.Word (Word8)

checkMask :: (Bits a, Eq a) => a -> a -> Bool
checkMask mask value =
  mask .&. value == mask

clamp :: Ord a => a -> a -> a -> a
clamp low high value
  | value <= low  = low
  | value >= high = high
  | otherwise     = value

combine :: Word8 -> Word8 -> Word8
combine high low =
  shiftL high 4 .|. low

pairs :: [a] -> [(a,a)]
pairs (x:y:rest) = (x,y) : pairs rest
pairs _ = []

discretize :: (Real a, Integral b) => a -> a -> b -> b -> a -> b
discretize srcMin srcMax tgtMin tgtMax value
  | value <= srcMin = tgtMin
  | value >= srcMax = tgtMax
  | otherwise = round $ fromIntegral tgtMin + frac * fromIntegral (tgtMax - tgtMin)
  where
    frac = (realToFrac (value - srcMin) / realToFrac (srcMax - srcMin)) :: Float

tripleZipWith :: (a -> b -> c) -> (a, a, a) -> (b, b, b) -> (c, c, c)
tripleZipWith f (a1, a2, a3) (b1, b2, b3) = (f a1 b1, f a2 b2, f a3 b3)

tripleMap :: (a -> b) -> (a, a, a) -> (b, b, b)
tripleMap f (a1, a2, a3) = (f a1, f a2, f a3)

littleEndianWord16Parser :: (Bits a, Num a) => Parser a
littleEndianWord16Parser = do
  byte0 <- fromIntegral <$> anyWord8
  byte1 <- fromIntegral <$> anyWord8
  pure $ shiftL byte1 8 .|. byte0

tripleParser :: (Bits a, Num a) => Parser (a, a, a)
tripleParser = do
  x <- littleEndianWord16Parser
  y <- littleEndianWord16Parser
  z <- littleEndianWord16Parser
  pure (x, y, z)