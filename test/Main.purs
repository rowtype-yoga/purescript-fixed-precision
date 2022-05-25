module Test.Main where

import Prelude

import Data.Fixed (class KnownPrecision, Fixed, PProxy, fromNumber, toNumber, fromString, reifyPrecision, reflectPrecision, toString, P100, rescale, fromBigInt, floor, ceil, numerator)
import Data.BigInt as BigInt
import Data.Maybe (Maybe(..), fromJust, isJust)
import Effect (Effect)
import Effect.Console (log)
import Data.Number as Math
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (arbitrary, quickCheck', (<?>), (===))
import Test.QuickCheck.Gen (Gen, chooseInt, uniform, suchThat)

-- Just here to package up the safe use of `unsafePartial`.
trying :: forall a b. Gen a -> (a -> Maybe b) -> Gen b
trying gen f = map (unsafePartial fromJust) (map f gen `suchThat` isJust)

genFixedP :: forall p. KnownPrecision p => PProxy p -> Gen (Fixed p)
genFixedP _ = map (fromNumber <<< stretch) uniform `trying` identity
  where
  -- maps [0,1] to [-10,10]
  stretch = (_ * 20.0) >>> (_ - 10.0)

fromNumberWith
  :: forall precision
  .  KnownPrecision precision
  => PProxy precision
  -> Number
  -> Maybe (Fixed precision)
fromNumberWith _ = fromNumber

-- | For type hinting.
withPrecisionOf
  :: forall precision
   . PProxy precision
  -> Fixed precision
  -> Fixed precision
withPrecisionOf _ x = x

truncateToPrecision
  :: forall precision
   . KnownPrecision precision
  => Int
  -> Fixed precision
  -> Fixed precision
truncateToPrecision p =
  (_ / scaleFactor)
  <<< (\x -> if x > zero then floor x else ceil x)
  <<< (_ * scaleFactor)
  where
  scaleFactor =
    fromBigInt (BigInt.pow (BigInt.fromInt 10) (BigInt.fromInt p))

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
  quickCheck' 1
    let x = fromString "" :: Maybe (Fixed P100)
    in x === Nothing

  log "Roundtrip: fromString <<< toString == Just"
  quickCheck' 1000 do
    precision <- chooseInt 0 100
    unsafePartial fromJust $ reifyPrecision precision \p -> do
      a <- genFixedP p
      let b = fromString (toString a)
      pure $ b == Just a <?> show { precision, a, b, numeratorA: numerator a, numeratorB: map numerator b }

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

  log ""
  log "rescale"
  log ""

  log "rescaling up then down should be no-op"
  quickCheck' 1000 do
    let lo = 10
    hi <- chooseInt 10 20
    unsafePartial fromJust $ join $
      reifyPrecision lo \lo_ ->
        reifyPrecision hi \hi_ -> do
          x <- genFixedP lo_
          let y = withPrecisionOf hi_ $ rescale x
          let xx = rescale y
          pure $ x == xx <?> show { x, xx, y }

  log "rescaling down then up should be the same as truncating"
  quickCheck' 1000 do
    let hi = 20
    lo <- chooseInt 10 20
    unsafePartial fromJust $ join $
      reifyPrecision lo \lo_ ->
        reifyPrecision hi \hi_ -> do
          x <- genFixedP hi_
          let y = withPrecisionOf lo_ $ rescale x
          let xx = truncateToPrecision lo x
          pure $ xx == rescale y <?> show { x, xx, y }
