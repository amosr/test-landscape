module RenderStrip where
import qualified Data.Vector.Unboxed as V

import Graphics.UI.GLUT
import qualified RenderUtils as R

type Heightmap = ((Float,Float) -> (Float,Float,Float))

{-# INLINE draw #-}
draw :: Heightmap       -- ^ heightmap
     -> (Float,Float)   -- ^ top
     -> (Float,Float)   -- ^ bottom
     -> (Float,Float)   -- ^ step
     -> IO ()
draw f (tx,ty) (bx,by) (sx,sy)
 = do   let v = line f (tx,ty) sy leny
        go tx v
 where
  leny = truncate ((by-ty) / sy)

  go x _
   | x >= bx = return ()

  go x v
   = do let v' = line f (x+sx,ty) sy leny
        draw_strips x v v'
        go (x+sx) v'

  draw_strips x v v'
   =  let vs = V.zip v v'
          vs'= V.unsafeTail vs
      in  renderPrimitive TriangleStrip --QuadStrip
        $ V.mapM_ (draw_verts x) (V.indexed $ V.zip vs vs')

  draw_verts x (i,((a1@(z1,_,_),b1@(z2,_,_)),(a2,b2)))
   =  do  let y = ty + sy * fromIntegral i
          normal1 z1 z2 a2
          draw_vert x      y a2
          normal2 z2 a2 b2
          draw_vert (x+sx) y b2
  
  -- cross (v2-v1) (v3-v1)
  -- v1 = (0,0,z1)
  -- v2 = (1,0,z2)
  -- v3 = (0,1,z3)
  --
  -- cross (u,v,w) (x,y,z)
  --  =  (v * z - w * y, ...)
  normal1 z1 z2 (z3,_,_)
   = normal (R.untuple3 R.n3 ((1,0,z2 - z1) `cross` (0,1,z3-z1)))

  -- cross (v2-v1) (v3-v1)
  -- v1 = (1,0,z1)
  -- v2 = (0,1,z2)
  -- v3 = (1,1,z3)
  normal2 z1 (z2,_,_) (z3,_,_)
   = normal (R.untuple3 R.n3 ((0,1,z3-z1) `cross` (-1,1,z2 - z1)))

  draw_vert x y (z,v,r)
   = do color  (R.c3 z v r)
        vertex (R.v3 x y z)
--        normal (R.n3 0 0 (-1))
--        putStrLn (show x ++ ", " ++ show y ++ ", " ++ show z)


{-# INLINE cross #-}
cross (u,v,w) (x,y,z) = ( v * z - w * y
                        , w * x - u * z
                        , u * y - v * x)


-- | get a single line along Y axis
{-# INLINE line #-}
line :: Heightmap
     -> (Float,Float) -- ^ start/top position
     -> Float         -- ^ Y step
     -> Int           -- ^ number of steps / size
     -> V.Vector (Float,Float,Float)
line f (tx,ty) sy len
 = V.generate len gen
 where
  gen i = f (tx, ty + sy * fromIntegral i)

