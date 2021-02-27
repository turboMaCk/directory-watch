module Main where

import qualified Spec.Basic as Basic
import Test.Hspec (hspec)


main :: IO ()
main =
    hspec $ Basic.spec
