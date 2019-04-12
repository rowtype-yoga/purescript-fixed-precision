module Test.Main where

import Prelude

import Data.Fixed (class KnownPrecision, Fixed, PProxy, fromNumber, fromString, reifyPrecision, toString, P100)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (arbitrary, quickCheck, quickCheck', (<?>), (===))
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
      pure $ (a + b) + c == a + (b + c) <?> show { precision, a, b, c }

  log "Identity: zero + a = a + zero = a"

  quickCheck' 1000 do
    precision <- chooseInt 0 10
    unsafePartial fromJust $ reifyPrecision precision \p -> do
      a <- genFixedP p
      pure $ zero + a == a <?> show { precision, a }
  quickCheck' 1000 do
    precision <- chooseInt 0 10
    unsafePartial fromJust $ reifyPrecision precision \p -> do
      a <- genFixedP p
      pure $ a + zero == a <?> show { precision, a }

  log "Commutative: a + b = b + a"
  quickCheck' 1000 do
    precision <- chooseInt 0 10
    unsafePartial fromJust $ reifyPrecision precision \p -> do
      a <- genFixedP p
      b <- genFixedP p
      pure $ a + b == b + a <?> show { precision, a, b }

  log "Multiplicative unit: one * a = a * one = a"

  quickCheck' 1000 do
    precision <- chooseInt 0 10
    unsafePartial fromJust $ reifyPrecision precision \p -> do
      a <- genFixedP p
      pure $ one * a == a <?> show { precision, a }
  quickCheck' 1000 do
    precision <- chooseInt 0 10
    unsafePartial fromJust $ reifyPrecision precision \p -> do
      a <- genFixedP p
      pure $ a * one == a <?> show { precision, a }

  log "Multiplicative zero: zero * a = a * zero = zero"
  quickCheck' 1000 do
    precision <- chooseInt 0 10
    unsafePartial fromJust $ reifyPrecision precision \p -> do
      a <- genFixedP p
      pure $ zero * a == zero <?> show { precision, a }
  quickCheck' 1000 do
    precision <- chooseInt 0 10
    unsafePartial fromJust $ reifyPrecision precision \p -> do
      a <- genFixedP p
      pure $ a * zero == zero <?> show { precision, a }

  log ""
  log "Parsing/printing"
  log ""

  log "An empty string parses Nothing"
  quickCheck
    let x = fromString "" :: Maybe (Fixed P100)
    in x === Nothing

  log "Roundtrip: fromString <<< toString == Just"
  quickCheck' 1000 do
    precision <- chooseInt 0 100
    unsafePartial fromJust $ reifyPrecision precision \p -> do
      a <- genFixedP p
      pure $ fromString (toString a) == Just a <?> show { precision, a }
