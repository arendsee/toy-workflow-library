import Prelude hiding(fail)
import ToyFlow

type ReportS = Report [String] 

-- Note the arguments are reversed (y x), this is too allow partial application
-- of the quotient in the monadic sequence
divide' :: Double -> Double -> ReportS Double
divide' y x
  | y == 0 = fail ["Division by 0 in " ++ expr]
  | y > 1000  = return (x / y) >>= warn ["denominator is over 1000 in " ++ expr]
  | otherwise = return (x / y) >>= note ["executed " ++ expr]
  where
    expr = "(" ++ show x ++ " / " ++ show y ++ ")"


main :: IO ()
main = do
  writeResultAndExit $
    divide' <$> (
                       divide' 10000 10  -- warning
                   >>= divide' 10        -- note
                   >>= divide' 10        -- note
                   >>= divide' 0         -- error
                   >>= divide' 0         -- error (this one will be skipped)
                )
            <*> divide' 0 1 -- error
