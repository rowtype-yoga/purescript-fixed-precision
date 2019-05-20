module Test.Main where

import Prelude

import Data.BigInt as BigInt
import Data.Fixed (class KnownPrecision, Fixed, PProxy, fromNumber, toNumber, fromString, reifyPrecision, reflectPrecision, toString)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Effect (Effect)
import Effect.Console (log)
import Math as Math
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (arbitrary, quickCheck', (<?>))
import Test.QuickCheck.Gen (Gen, chooseInt, suchThat)

-- Just here to package up the safe use of `unsafePartial`.
trying :: forall a b. Gen a -> (a -> Maybe b) -> Gen b
trying gen f = map (unsafePartial fromJust) (map f gen `suchThat` isJust)

genFixedP :: forall p. KnownPrecision p => PProxy p -> Gen (Fixed p)
genFixedP _ = map fromNumber arbitrary `trying` identity

fromNumberWith
  :: forall precision
  .  KnownPrecision precision
  => PProxy precision
  -> Number
  -> Maybe (Fixed precision)
fromNumberWith _ = fromNumber

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

  log "Roundtrip: fromString <<< toString == Just"
  quickCheck' 1000 do
    precision <- chooseInt 0 100
    unsafePartial fromJust $ reifyPrecision precision \p -> do
      a <- genFixedP p
      pure $ fromString (toString a) == Just a <?> show { precision, a }

  log ""
  log "Conversions to/from Number"
  log ""

  log "Roundtrip inaccuracy should be bounded by precision"
  quickCheck' 1000 \x -> do
    -- This is limited by the precision of Number
    precision <- chooseInt 0 10
    pure $ unsafePartial fromJust $ reifyPrecision precision \p -> do
      -- Note: we use 0.5 here because these numbers should differ by no more
      -- than half of the distance between two consecutive representable values
      -- of the type `Fixed precision`.
      let epsilon = 0.5 / BigInt.toNumber (reflectPrecision p)
      case toNumber <$> fromNumberWith p x of
        Just x' ->
          (Math.abs (x' - x) <= epsilon)
          <?> show { precision, epsilon, x, x' }
        Nothing ->
          false
          <?> ("failed to roundtrip via Number: " <> show { precision, x })
