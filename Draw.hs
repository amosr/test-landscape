module Draw where
import qualified Generator   as G
import qualified RenderStrip as RS

draw res = RS.draw G.heightmap_sinerot (-15,-15) (15,15) (res, res) 
