import System.IO

import ToyFlow (writeResult)
import ToyFlow.Filters

main :: IO ()
main = do
  hPutStrLn stderr "-----------------------------------------"
  writeResult $ return 100.0 >>= dividenode 2.0 >>= dividenode 10.0
                           >>= dividenode 0.0 >>= dividenode 10.0

  hPutStrLn stderr "-----------------------------------------"
  writeResult $ return 100.0 >>= dividenode 2.0  >>= dividenode 10.0
                           >>= dividenode 10.0 >>= dividenode 10.0
                           >>= couplenode "foo"
