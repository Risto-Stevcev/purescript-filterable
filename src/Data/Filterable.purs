module Data.Filterable where

import Prelude
import Data.Array (filter) as Array
import Data.Foldable (class Foldable, foldl, length)
import Data.BooleanEq (class BooleanEq, toBoolean)
import Data.HeytingAlgebra (tt)
import Data.List (List)
import Data.List (filter) as List
import Data.Map (Map)
import Data.Map (filter) as Map
import Data.StrMap (StrMap)
import Data.StrMap (filter) as StrMap
import Data.Monoid (class Monoid, mempty)

-- | The `Filterable` class provides a `filter`ing operation for `Foldable` structures.
-- |
-- | Instances must satisfy the following laws in addition to `Foldable` laws:
-- |
-- | - `filter (\_ → true) x == x`
-- | - `filter (\_ → false) x == mempty`
-- | - `length (filter f x) <= length x`
class Foldable f ⇐ Filterable f where
  filter ∷ ∀ a h. BooleanEq h ⇒ (a → h) → f a → f a

verifyFilter
  ∷ ∀ f h a. Eq (f a) ⇒ Monoid (f a) ⇒ BooleanEq h ⇒ Filterable f
  ⇒ (a → h) → f a → Boolean
verifyFilter f x = (filter (\_ → true) x == x)
                && (filter (\_ → false) x == mempty)
                && (length (filter f x) <= (length x) ∷ Int)

defaultFilter
  ∷ ∀ f h a. BooleanEq h ⇒ Applicative f ⇒ Foldable f ⇒ Monoid (f a)
  ⇒ (a → h) → f a → f a
defaultFilter f x = foldl (\acc e → if f e == tt then acc <> (pure e) else acc) mempty x

instance filterableArray ∷ Filterable Array where
  filter f = Array.filter (toBoolean <<< f)

instance filterableList ∷ Filterable List where
  filter f = List.filter (toBoolean <<< f)

instance filterableMap ∷ Ord k => Filterable (Map k) where
  filter f = Map.filter (toBoolean <<< f)

instance filterableStrMap ∷ Filterable StrMap where
  filter f = StrMap.filter (toBoolean <<< f)
