module ToyFlow
(
    writeResult
  , writeResultAndExit
  , module ToyFlow.Report
) where

import Prelude hiding(fail)
import System.IO
import qualified System.Exit as SE

import ToyFlow.Report

writeResult :: (Monoid e, ShowE e, Show o) => Report e o -> IO ()
writeResult (Pass x w n)
  =  hPutStr stdout (show x)
  >> hPutStr stderr (show3E mempty w n)
writeResult (Fail e w n)
  =  hPutStr stderr (show3E e w n)

writeResultAndExit :: (Monoid e, ShowE e, Show o) => Report e o -> IO a
writeResultAndExit (Pass x w n)
  =  writeResult (Pass x w n)
  >> SE.exitWith SE.ExitSuccess
writeResultAndExit f
  =  writeResult f
  >> SE.exitWith (SE.ExitFailure 1)
