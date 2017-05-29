import System.IO

import ToyFlow (writeResult)
import ToyFlow.Filters

main :: IO ()
main = do
  hPutStrLn stderr "-----------------------------------------"
  writeResult $ return 100 >>= trydivide' 2 >>= trydivide' 10
                           >>= trydivide' 0 >>= trydivide' 10

  hPutStrLn stderr "-----------------------------------------"
  writeResult $ return 100 >>= trydivide' 2  >>= trydivide' 10
                           >>= trydivide' 10 >>= trydivide' 10
                           >>= couple' "foo"
