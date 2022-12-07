module Main (main) where

import Props.Data.Bounds (props)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Property tests"
      [ props
      ]
