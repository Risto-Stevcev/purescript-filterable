module Test.Data.Filterable where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Gen (elements)
import Control.Monad.Gen.Class (class MonadGen, chooseInt)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Char (fromCharCode)
import Data.Filterable (verifyFilter)
import Data.Int (floor)
import Data.List (List)
import Data.Map.Gen (genMap)
import Data.NonEmpty ((:|))
import Data.StrMap.Gen (genStrMap)
import Data.String.Gen (genAsciiString)
import Global (infinity)
import Test.QuickCheck.Gen (Gen)
import Test.Spec (describe, it)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

data Foo = Bar | Baz

instance eqFoo ∷ Eq Foo where
  eq Bar Bar = true
  eq Baz Baz = true
  eq _   _   = false

instance semigroupFoo ∷ Semigroup Foo where
  append Bar a = Bar
  append Baz Bar = Bar
  append Baz Baz = Baz

genFoo ∷ ∀ m. MonadRec m ⇒ MonadGen m ⇒ m Foo
genFoo = elements (Bar :| [Baz])

genChar ∷ ∀ m. MonadRec m ⇒ MonadGen m ⇒ m Char
genChar = fromCharCode <$> chooseInt 0 65535

isEven ∷ Int → Boolean
isEven = (eq 0) <<< (_ `mod` 2)

posInfinity ∷ Int
posInfinity = floor infinity

negInfinity ∷ Int
negInfinity = (negate <<< floor) infinity

genInt ∷ ∀ m. MonadRec m ⇒ MonadGen m ⇒ m Int
genInt = chooseInt negInfinity posInfinity

verifyMap ∷ Gen Boolean
verifyMap = do
  sample ← genMap genChar genInt
  pure $ verifyFilter isEven sample

verifyStrMap ∷ Gen Boolean
verifyStrMap = do
  sample ← genStrMap genAsciiString genFoo
  pure $ verifyFilter ((==) Bar) sample

main ∷ Eff (QCRunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "Filterable Array" do
    it "should satisfy the laws" do
      quickCheck \(x ∷ Array Int) → verifyFilter isEven x

  describe "Filterable List" do
    it "should satisfy the laws" do
      quickCheck \(x ∷ List Int) → verifyFilter isEven x

  describe "Filterable Map" do
    it "should satisfy the laws" do
      quickCheck verifyMap

  describe "Filterable StrMap" do
    it "should satisfy the laws" do
      quickCheck verifyStrMap
