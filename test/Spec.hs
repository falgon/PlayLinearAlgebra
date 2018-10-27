import Utils
import Data.Maybe (fromJust)

main :: IO ()
main = print $ ($ 1) $ fromJust $ explicitFn [[2],[3],[4]]
