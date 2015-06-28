module Main where

  import qualified Lindenmayer.Config as Config
  import qualified Lindenmayer.System as Sys
  import qualified Lindenmayer.Graphics as Gra
  import System.Environment
  import Control.Monad (when)

  verifySystem :: Config.Config -> Bool
  verifySystem c = 
    let s        = Config.sys c
    in  Sys.verifySystem ((Config.voc   s),
                          (Config.const s), 
                          (Config.om    s),
                          (Config.rule  s))
  main :: IO ()
  main = do
    args <- getArgs
    let p = case args of
            []     -> error "Give a Lindenmayer System configuration file!"
            (a:[]) -> a
            _      -> error 
                      "Give me just one Lindenmayer System configuration file!"
    s <- readFile p
    putStrLn s
    let c = Config.configure s
    when (verifySystem c) $ Gra.drawSystem c 
