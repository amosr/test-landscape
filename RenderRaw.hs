module RenderRaw where
import Graphics.Rendering.OpenGL.Raw

enableColorMaterial :: IO ()
enableColorMaterial = glEnable gl_COLOR_MATERIAL
