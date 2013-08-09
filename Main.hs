import Control.Monad ( when )
import Data.IORef ( IORef, newIORef )
import Data.List ( intersperse )
import System.Console.GetOpt
import System.Exit ( exitWith, ExitCode(ExitSuccess), exitFailure )
import Graphics.UI.GLUT

import qualified Draw        as D
import qualified Generator   as G
import qualified RenderStrip as RS
import qualified RenderUtils as R
import qualified RenderRaw   as RR

--------------------------------------------------------------------------------

type View = (GLfloat, GLfloat, GLfloat)

data State = State {
   frames  :: IORef Int,
   t0      :: IORef Int,
   viewRot :: IORef View,
   res     :: IORef Float,
   angle'  :: IORef GLfloat }

makeState :: IO State
makeState = do
   f <- newIORef 0
   t <- newIORef 0
   v <- newIORef (20, 30, 0)
   r <- newIORef 0.1
   a <- newIORef 0
   return $ State { frames = f, t0 = t, viewRot = v, res = r, angle' = a }


draw :: State -> IO ()
draw state = do
   clear [ ColorBuffer, DepthBuffer ]
   (x, y, z) <- get (viewRot state)
   a <- get (angle' state)
   r <- get (res state)

   preservingMatrix $ do
      rotate x (Vector3 1 0 0)
      rotate y (Vector3 0 1 0)
      rotate z (Vector3 0 0 1)

      -- rotate (a/5) (Vector3 1 0.5 0.2)

      D.draw r

   swapBuffers
   frames state $~! (+1)
   t0' <- get (t0 state)
   t <- get elapsedTime
   when (t - t0' >= 5000) $ do
      f <- get (frames state)
      let seconds = fromIntegral (t - t0') / 1000 :: GLfloat
          fps = fromIntegral f / seconds
      putStrLn (show f ++ " frames in " ++ show seconds ++ " seconds = "++ show fps ++ " FPS")
      t0 state $= t
      frames state $= 0


idle :: State -> IdleCallback
idle state = do
   angle' state $~! (+2)
   reportErrors
   postRedisplay Nothing

keyboard :: State -> KeyboardMouseCallback
keyboard state (Char 'z')           _ _ _ = modRot state ( 0,  0,  5)
keyboard state (Char 'Z')           _ _ _ = modRot state ( 0,  0, -5)
keyboard state (SpecialKey KeyUp)   _ _ _ = modRot state ( 5,  0,  0)
keyboard state (SpecialKey KeyDown) _ _ _ = modRot state (-5,  0,  0)
keyboard state (SpecialKey KeyLeft) _ _ _ = modRot state ( 0,  5,  0)
keyboard state (SpecialKey KeyRight)_ _ _ = modRot state ( 0, -5,  0)
keyboard state (Char '+')           _ _ _ = res state $~! (*1.5)
keyboard state (Char '-')           _ _ _ = res state $~! (/1.5)
keyboard _     (Char '\27')         _ _ _ = exitWith ExitSuccess
keyboard _     _                    _ _ _ = return ()

modRot :: State -> View -> IO ()
modRot state (dx,dy,dz) = do
   (x, y, z) <- get (viewRot state)
   viewRot state $= (x + dx, y + dy, z + dz)
   postRedisplay Nothing

-- new window size or exposure
reshape :: ReshapeCallback
reshape s@(Size width height) = do
   let h = fromIntegral height / fromIntegral width

   viewport $= (Position 0 0, s)
   matrixMode $= Projection
   loadIdentity
   frustum (-1) 1 (-h) h 5 60
   matrixMode $= Modelview 0
   loadIdentity
   translate (Vector3 0 0 (-40 :: GLfloat))

data Flag = GLInfo | AutoExit deriving ( Eq, Ord, Show )

argInfo :: [OptDescr Flag]
argInfo  = [
   Option ['i'] ["info"] (NoArg GLInfo)   "print gl information",
   Option ['e'] ["exit"] (NoArg AutoExit) "auto exit after 30 seconds" ]

opts :: [String] -> IO [Flag]
opts args =
   case getOpt Permute argInfo args of
      (o,_,[])   -> return o
      (_,_,errs) -> do
         putStr (concat errs ++ usageInfo "Usage: Gears [OPTION...]" argInfo)
         exitFailure

info :: IO ()
info = do
   rendererStr <- get renderer
   putStrLn ("GL_RENDERER   = " ++ rendererStr)
   vendorStr <- get vendor
   putStrLn ("GL_VENDOR     = " ++ vendorStr)
   versionStr <- get glVersion
   putStrLn ("GL_VERSION    = " ++ versionStr)
   exts <- get glExtensions
   putStrLn ("GL_EXTENSIONS = " ++ concat (intersperse " " exts))

myInit :: [String] -> IO ()
myInit args = do
   position (Light 0) $= Vertex4 5 5 10 0
   cullFace $= Nothing --Just Front -- FrontAndBack
   lighting $= Enabled
   light (Light 0) $= Enabled
   depthFunc $= Just Less
   shadeModel $= Smooth
   --light2

   normalize $= Enabled

   RR.enableColorMaterial

   flags <- opts args
   when (GLInfo `elem` flags) info

   return ()


light2 :: IO ()
light2 = do
   let l = Light 1
   position l $= Vertex4 (-5) (-20) (-10) 0
   ambient  l $= Color4 0.2 0.2 0.2 1.0
   diffuse  l $= Color4 0.5 0.2 0.1 1.0
   light l $= Enabled


visible :: State -> Visibility -> IO ()
visible state Visible    = idleCallback $= Just (idle state)
visible _     NotVisible = idleCallback $= Nothing

main :: IO ()
main  = do
   (_progName, args) <- getArgsAndInitialize
   initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]

   initialWindowPosition $= Position 0 0
   initialWindowSize $= Size 300 300
   _ <- createWindow "Gears"
   state <- makeState
   myInit args

   displayCallback $= draw state
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboard state)
   visibilityCallback $= Just (visible state)

   mainLoop

