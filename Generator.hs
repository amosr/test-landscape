{-# LANGUAGE BangPatterns #-}
module Generator where
import qualified Data.Vector.Unboxed as V
import System.IO.Unsafe
import System.Random

-- | height, vegetation and rockiness
type Result = (Float,Float,Float)

{-# INLINE heightmap2 #-}
heightmap2 :: (Float,Float) -> Result
heightmap2 (!x,!y)
 = (height,veg,rock)
 where
  (h,v,r) = heightmap (x,y)
  height  = (sin h) + lerpy 1 10 1
  veg     = v
  rock    = r
  lerpy si off so = (lerp2 noise (x*si + off) (y*si + off)) * so


{-# INLINE heightmap #-}
heightmap :: (Float,Float) -> Result
heightmap (!x,!y)
 = (height,veg,rock)
 where
    lerpy si off so = (lerp2 noise (x*si + off) (y*si + off)) * so
    height = lerpy 10 10 0.2 + lerpy 05 05 0.4 + lerpy 01 01 1.2 + lerpy 0.2 0.2 1.5
    veg    = lerpy 05 05 0.3 + lerpy 01 01 0.4
    rock   = lerpy 05 05 0.5 + lerpy 01 01 0.6


{-# INLINE heightmap_sine #-}
heightmap_sine :: (Float,Float) -> Result
heightmap_sine (!x,!y)
 = (sin x, sin x, sin y)

{-# INLINE heightmap_sinerot #-}
heightmap_sinerot :: (Float,Float) -> Result
heightmap_sinerot (!x,!y)
 = (height,veg,rock)
 where
  veg = lerpy 0.6 1 1
  rock = lerpy 0.4 2 1

  lerpy si off so = (lerp2 noise (x*si + off) (y*si + off)) * so
  mag = (lerpy 10 10 0.2 * rock) + (lerpy 05 05 0.4 * (1 - veg)) + lerpy 01 01 1.2 + lerpy 0.2 0.2 1.5
  dir = lerpy 0.2 1 6.1415 -- lerpy 10 20 0.2 + lerpy 05 35 0.4 + lerpy 01 51 1.2 + lerpy 0.2 1.2 1.5

  height  = mag * sin dir * cos dir

{-# INLINE lerp2 #-}
lerp2 :: V.Vector Float -> Float -> Float -> Float
lerp2 !n !x !y
 = let !len  = noiseSize -- V.length n
       !lenY = noiseSizeY -- truncate $ sqrt $ fromIntegral len
       !x'  = floor x -- damn it !!! was using truncate here, issues with negatives.
       !y'  = floor y
       !xf  = x - (fromIntegral x')
       !yf  = y - (fromIntegral y')
       !xfm = 1 - xf
       !yfm = 1 - yf

       ixBase = (x' + y'*lenY)

       ix extra = n `V.unsafeIndex` ((ixBase + extra) `mod` len)

       !c00 = ix 0
       !c01 = ix lenY
       !c10 = ix 1
       !c11 = ix (lenY + 1)
   in c00 * xfm * yfm
    + c01 * xfm * yf
    + c10 * xf  * yfm
    + c11 * xf  * yf


-- cheap crappy noise generator I don't even know what I'm doing
noiseSize :: Int
noiseSize   = 263
noiseSizeY :: Int
noiseSizeY  = 29

noise :: V.Vector Float
noise
 = unsafePerformIO
 $ V.generateM noiseSize (const randomIO)

