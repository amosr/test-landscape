module RenderUtils where
import Graphics.UI.GLUT

import qualified Unsafe.Coerce as UC

{-# INLINE f2gl #-}
f2gl :: Float -> GLfloat
f2gl = UC.unsafeCoerce

{-# INLINE gl2f #-}
gl2f :: GLfloat -> Float
gl2f = UC.unsafeCoerce

{-# INLINE on3 #-}
on3 :: (a -> a -> a -> b) -> (c -> a) -> (c -> c -> c -> b)
on3 con f x y z
 = con (f x) (f y) (f z)

{-# INLINE on4 #-}
on4 :: (a -> a -> a -> a -> b) -> (c -> a) -> (c -> c -> c -> c -> b)
on4 con f w x y z
 = con (f w) (f x) (f y) (f z)


{-# INLINE v3 #-}
v3 :: Float -> Float -> Float -> Vertex3 GLfloat
v3 = Vertex3 `on3` f2gl


{-# INLINE c3 #-}
c3 :: Float -> Float -> Float -> Color3 GLfloat
c3 = Color3 `on3` f2gl

{-# INLINE c4 #-}
c4 :: Float -> Float -> Float -> Float -> Color4 GLfloat
c4 = Color4 `on4` f2gl


{-# INLINE n3 #-}
n3 :: Float -> Float -> Float -> Normal3 GLfloat
n3 = Normal3 `on3` f2gl

{-# INLINE untuple3 #-}
untuple3 :: (a -> a -> a -> b) -> (a,a,a) -> b
untuple3 f (a,b,c) = f a b c

