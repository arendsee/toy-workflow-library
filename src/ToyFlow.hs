module ToyFlow (writeResult) where

import System.IO

import ToyFlow.Node

writeResult :: (Show e, Show o) => Node e o -> IO ()
writeResult (Node (Left e)) = hPutStr stderr (show e)
writeResult (Node (Right (e,o))) =
  hPutStr stderr (show e) >> hPutStrLn stdout (show o)
