module Plot
where

import Graphics.Gnuplot.Simple
import qualified Graphics.Gnuplot.Value.Atom as A
import qualified Graphics.Gnuplot.Value.Tuple as T


plot :: (Fractional a, A.C a, T.C a) => (a,a) -> (a -> a) -> IO ()
plot scale = plotFunc [] (linearScale 1000 scale) 
