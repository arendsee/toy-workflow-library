module ToyFlow (writeResult) where

import System.IO

import ToyFlow.Node

writeResult :: (Show e, Show o) => Node e o -> IO ()
writeResult (Node e Nothing) = hPutStr stderr (show e)
writeResult (Node e (Just o)) =
  hPutStr stderr (show e) >> hPutStrLn stdout (show o)
