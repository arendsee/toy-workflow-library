module ToyFlow
(
    writeResult
  , module ToyFlow.Report
) where

import System.IO

import Prelude hiding(fail)
import ToyFlow.Report

writeResult :: (Monoid e, ShowE e, Show o) => Report e o -> IO ()
writeResult (Pass x w n)
  =  hPutStr stdout (show x)
  >> hPutStr stderr (show3E mempty w n)
writeResult (Fail e w n)
  =  hPutStr stderr (show3E e w n)
