module Main (main) where

import Data.Data (Proxy (..))
import Props.Data.Bounds (props)
import Props.MaxRuns (MaxRuns)
import Test.Tasty
import Test.Tasty.Options (OptionDescription (..))

main :: IO ()
main = do
  let options = includingOptions [Option @MaxRuns Proxy]
      ingredients = options : defaultIngredients
  defaultMainWithIngredients ingredients $
    testGroup
      "Property tests"
      [ props
      ]
