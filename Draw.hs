module Draw where
import qualified Generator   as G
import qualified RenderStrip as RS

draw res = RS.draw G.heightmap_sinerot (-10,-10) (10,10) (res, res) 
