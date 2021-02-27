module Main where

import qualified Spec.Basic as Basic
import Test.Hspec (hspec, context)


main :: IO ()
main =
    hspec $ context "Spec.Basic" Basic.spec
