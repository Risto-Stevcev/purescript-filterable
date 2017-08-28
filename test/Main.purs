module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Test.Spec.QuickCheck (QCRunnerEffects)
import Test.Data.Filterable (main) as Test

main :: Eff (QCRunnerEffects ()) Unit
main = Test.main
