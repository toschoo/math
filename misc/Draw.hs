module Draw -- Graphics.Concept.Draw
where

  import qualified Graphics.GD as GD
  import           Data.Tree
  import           Data.List (delete,sort,find,foldl')
  import           Control.Monad (void)
  import           Control.Applicative ((<$>))
  import           System.FilePath.Posix ((</>))

  import Debug.Trace (trace)

  white,black :: GD.Color
  white = GD.rgb 255 255 255
  black = GD.rgb   0   0   0
  red   = GD.rgb 255   0   0
  green = GD.rgb   0 255   0
  blue  = GD.rgb   0   0 255

  data Vertex = Vertex {
                  vtxText :: String,
                  vtxCol  :: GD.Color,
                  vtxP    :: GD.Point
                }
    deriving (Show)

  instance Eq Vertex where
    (Vertex t1 _ _) == (Vertex t2 _ _) = t1 == t2

  data Edge   = Edge {
                  edgColor :: GD.Color,
                  edgP1    :: Vertex,
                  edgP2    :: Vertex
                }
    deriving (Show)

  instance Eq Edge where
    (Edge _ p11 p12) == (Edge _ p21 p22) = p11 == p21 &&
                                           p12 == p22

  edgPP1 :: Edge -> GD.Point
  edgPP1 = vtxP . edgP1

  edgPP2 :: Edge -> GD.Point
  edgPP2 = vtxP . edgP2

  edgP1Text :: Edge -> String
  edgP1Text = vtxText . edgP1

  edgP2Text :: Edge -> String
  edgP2Text = vtxText . edgP2

  data Region = Region {
                   regImg    :: GD.Image,
                   regBase   :: GD.Point,
                   regSz     :: GD.Size,
                   regColor  :: GD.Color
                }

  data Graph = Graph {
                grpFontDir  :: FilePath,
                grpFont     :: FilePath,
                grpFontSz   :: Int,
                grpRegion   :: Region,
                grpVertices :: [Vertex],
                grpEdges    :: [Edge]
              }

  stdRegion :: GD.Image -> GD.Size -> Region
  stdRegion img sz = Region {
                       regImg   = img,
                       regBase  = (0,0),
                       regSz    = sz,
                       regColor = black
                     }

  stdGraph :: Region -> Graph
  stdGraph reg = Graph {
                   -- grpFontDir  = "/usr/share/fonts/truetype/msttcorefonts",
                   grpFontDir  = "/usr/share/fonts/truetype/asana-math",
                   -- grpFont     = "arial.ttf",
                   grpFont     = "Asana-Math.otf",
                   grpFontSz   = 10,
                   grpRegion   = reg,
                   grpVertices = [],
                   grpEdges    = []
                 }

  grpRegSz :: Graph -> GD.Size
  grpRegSz = regSz . grpRegion

  grpXMargin :: Graph -> Int
  grpXMargin = fst . grpMargin

  grpYMargin :: Graph -> Int
  grpYMargin = snd . grpMargin

  grpMargin :: Graph -> GD.Point
  grpMargin = regBase . grpRegion

  grpBgColor :: Graph -> GD.Color
  grpBgColor = regColor . grpRegion

  addVertices :: [Vertex] -> Graph -> Graph
  addVertices vs gr = gr{grpVertices = grpVertices gr ++ vs}

  addEdges :: [Edge] -> Graph -> Graph
  addEdges es gr = gr{grpEdges = grpEdges gr ++ es}

  mark :: [String] -> GD.Color -> Graph -> Graph -- no backtracking!
  mark [] c g = g
  mark xs c g = g{grpEdges = go xs $ grpEdges g}
    where go [] es  = es
          go [n] es = es 
          go ns []  = grpEdges g
          go (h:n:ns) es = case find (a2b h n) es of
                             Nothing -> grpEdges g
                             Just e  -> let e' = Edge c (edgP1 e) (edgP2 e)
                                         in go (n:ns) $ e' : delete e es
          a2b s t e = edgP1Text e == s &&
                      edgP2Text e == t 

  markN :: [[String]] -> GD.Color -> Graph -> Graph
  markN ts c g0 = foldl' (\g t -> mark t c g) g0 ts

  -------------------------------------------------------------------------
  -- Draw: image, size of tree, number of generations, tree
  -------------------------------------------------------------------------
  tree2graph :: (Show a) => Graph -> Int -> Tree a -> Graph
  tree2graph gr0 g t = go gr0 1 [] 
    where go gr i ps | i > g = gr
                     | otherwise = let ks  = map getNode (getGeneration i t)
                                       vs  = getVertices gr g i ks
                                       es  = getEdges i t ps vs 
                                       gr' = addVertices vs $ addEdges es $ gr
                                     in go gr' (i+1) vs

  drawGraph :: Double -> Graph -> IO ()
  drawGraph fs gr = let f   = grpFontDir gr </> grpFont gr 
                        img = regImg $ grpRegion gr 
                     in do
    mapM_ (\e -> GD.drawLine (edgPP1 e) (edgPP2 e) (edgColor e) img) $ grpEdges gr
    mapM_ (\v -> do
      let (x,y) = vtxP v
      (p1,_,p3,_) <- GD.measureString f fs 0 (x,y)
                                             (vtxText v) white
      -- putStrLn(show p1 ++ " -> " ++ show p3 ++ " (" ++ show fs ++ ")")
      GD.drawFilledRectangle p1 p3 (grpBgColor gr) img
      void $ GD.drawString f fs 0 (vtxP    v) 
                                  (vtxText v) red   img) $ grpVertices gr

  savePng :: FilePath -> Graph -> IO ()
  savePng p gr = GD.savePngFile p $ regImg (grpRegion gr)

  getGeneration :: Int -> Tree a -> [Tree a]
  getGeneration 1 n = [n]
  getGeneration g (Node _ ks) = concatMap (getGeneration (g-1)) ks 

  getNode :: Tree a -> a
  getNode (Node n _) = n

  getVertices :: (Show a) => Graph -> Int -> Int -> [a] -> [Vertex]
  getVertices gr g i t = let sz      = grpRegSz gr
                             (x',y') = nextG g i sz
                             (x,y)   = (x'+grpXMargin gr, y'+grpYMargin gr)
                             d       = dist  g i sz
                          in go 1 (x,y) d t
    where go _ _ _ [] = []
          go i (x1,y1) d (n:ns) = 
            let v = Vertex (show n) (grpBgColor gr) (x1,y1)
             in v : go (i+1) (x1+d,y1) d ns

  getEdges :: Int -> Tree a -> [Vertex] -> [Vertex] -> [Edge]
  getEdges 1 _ _  _  = []
  getEdges g t ps vs = go (getGeneration (g-1) t) ps vs
    where go [] _ _  = []
          go _ [] _  = []
          go _ _ []  = []
          go (n:ns) (p1:xs) (p2:zs) =
            let e = Edge white p1 p2
             in case n of
                  Node _ []     ->     go ns (p1:xs) (p2:zs)
                  Node _ [k]    -> e : go ns xs zs
                  Node v (k:ks) -> e : go ((Node v ks):ns) (p1:xs) zs 

  nextG :: Int -> Int -> GD.Size -> GD.Point
  nextG g i (sx,sy) = calc (sx,sy)
    where calc (x,y) = let h  = x `div` (2^i)
                           v  = (i-1) * (y `div` g)
                      in (h,v) -- + margin

  dist :: Int -> Int -> GD.Size -> Int
  dist g i (x,_) = let n  = 2^(i-1)
                    in x `div` n

