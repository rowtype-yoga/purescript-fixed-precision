module Test.Main where

import Prelude

import Data.Fixed (class KnownPrecision, Fixed, PProxy, fromNumber, reifyPrecision)
import Data.Maybe (Maybe, fromJust, isJust)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (arbitrary, quickCheck', (<?>))
import Test.QuickCheck.Gen (Gen, chooseInt, suchThat)

-- Just here to package up the safe use of `unsafePartial`.
trying :: forall a b. Gen a -> (a -> Maybe b) -> Gen b
trying gen f = map (unsafePartial fromJust) (map f gen `suchThat` isJust)

genFixedP :: forall p. KnownPrecision p => PProxy p -> Gen (Fixed p)
genFixedP _ = map fromNumber arbitrary `trying` identity

main :: Effect Unit
main = do
  log "Commutative monoid under addition:"
  log ""

  log "Associativity: (a + b) + c = a + (b + c)"
  quickCheck' 1000 do
    precision <- chooseInt 0 10
    unsafePartial fromJust $ reifyPrecision precision \p -> do
      a <- genFixedP p
      b <- genFixedP p
      c <- genFixedP p
      pure $ (a + b) + c == a + (b + c) <?> show { a, b, c }

  log "Identity: zero + a = a + zero = a"

  quickCheck' 1000 do
    precision <- chooseInt 0 10
    unsafePartial fromJust $ reifyPrecision precision \p -> do
      a <- genFixedP p
      pure $ zero + a == a <?> show { a }
  quickCheck' 1000 do
    precision <- chooseInt 0 10
    unsafePartial fromJust $ reifyPrecision precision \p -> do
      a <- genFixedP p
      pure $ a + zero == a <?> show { a }

  log "Commutative: a + b = b + a"
  quickCheck' 1000 do
    precision <- chooseInt 0 10
    unsafePartial fromJust $ reifyPrecision precision \p -> do
      a <- genFixedP p
      b <- genFixedP p
      pure $ a + b == b + a <?> show { a, b }

  log "Multiplicative unit: one * a = a * one = a"

  quickCheck' 1000 do
    precision <- chooseInt 0 10
    unsafePartial fromJust $ reifyPrecision precision \p -> do
      a <- genFixedP p
      pure $ one * a == a <?> show { a }
  quickCheck' 1000 do
    precision <- chooseInt 0 10
    unsafePartial fromJust $ reifyPrecision precision \p -> do
      a <- genFixedP p
      pure $ a * one == a <?> show { a }

  log "Multiplicative zero: zero * a = a * zero = zero"
  quickCheck' 1000 do
    precision <- chooseInt 0 10
    unsafePartial fromJust $ reifyPrecision precision \p -> do
      a <- genFixedP p
      pure $ zero * a == zero <?> show { a }
  quickCheck' 1000 do
    precision <- chooseInt 0 10
    unsafePartial fromJust $ reifyPrecision precision \p -> do
      a <- genFixedP p
      pure $ a * zero == zero <?> show { a }
