-- | This module defines a numeric type `Fixed` for working with
-- | fixed point numbers in base 10. The precision is tracked in
-- | the types.

module Data.Fixed
  ( Fixed
  , fromInt
  , fromNumber
  , toNumber
  , fromString
  , toString
  , toStringWithPrecision
  , numerator
  , floor
  , ceil
  , round
  , approxDiv
  , kind Precision
  , One
  , TenTimes
  , P1
  , P10
  , P100
  , P1000
  , P10000
  , P100000
  , P1000000
  , PProxy(..)
  , class KnownPrecision
  , reflectPrecision
  , reflectPrecisionDecimalPlaces
  , reifyPrecision
  ) where

import Prelude

import Control.MonadZero (guard)
import Data.Array (replicate)
import Data.BigInt as BigInt
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Monoid as Monoid
import Data.String.CodeUnits as StringCU
import Math as Math

-- | A kind for type-level precision information
foreign import kind Precision

-- | No decimal places
foreign import data One :: Precision

-- | One more decimal place
foreign import data TenTimes :: Precision -> Precision

type P1 = One

-- | One decimal place
type P10 = TenTimes P1

-- | Two decimal places
type P100 = TenTimes P10

-- | Three decimal places
type P1000 = TenTimes P100

-- | Four decimal places
type P10000 = TenTimes P1000

-- | Five decimal places
type P100000 = TenTimes P10000

-- | Six decimal places
type P1000000 = TenTimes P100000

-- | A value-level proxy for a type-level precision.
data PProxy (precision :: Precision) = PProxy

-- | Precision which is known, i.e. it can be reflected to a
-- | value at runtime, given a `PProxy`.
-- |
-- | `reflectPrecision` returns a multiple of ten, corresponding
-- | to the maximum number of decimal places which can be stored.
-- |
-- | ```
-- | > reflectPrecision (PProxy :: PProxy P1000)
-- | 1000
-- | ```
class KnownPrecision (precision :: Precision) where
  reflectPrecision :: PProxy precision -> Int

instance knownPrecisionOne :: KnownPrecision One where
  reflectPrecision _ = 1

instance knownPrecisionTenTimes :: KnownPrecision p => KnownPrecision (TenTimes p) where
  reflectPrecision _ = 10 * reflectPrecision (PProxy :: PProxy p)

-- | Get the number of decimal places associated with a given `Precision` at
-- | the value level.
-- |
-- | ```
-- | > reflectPrecisionDecimalPlaces (PProxy :: PProxy P1000)
-- | 3
-- | ```
reflectPrecisionDecimalPlaces
  :: forall precision
   . KnownPrecision precision
  => PProxy precision
  -> Int
reflectPrecisionDecimalPlaces _ =
  let
    p = reflectPrecision (PProxy :: PProxy precision)
  in
    Int.round (Math.log (Int.toNumber p) / Math.ln10)


-- | Reify an non-negative integer (a power of ten) as a `Precision`.
-- |
-- | For example
-- |
-- | ```
-- | > reifyPrecision 0 reflectPrecision
-- | Just 1
-- | > reifyPrecision 1 reflectPrecision
-- | Just 10
-- | > reifyPrecision 2 reflectPrecision
-- | Just 100
-- | > reifyPrecision (-1) reflectPrecision
-- | Nothing
-- | ```
reifyPrecision :: forall r. Int -> (forall precision. KnownPrecision precision => PProxy precision -> r) -> Maybe r
reifyPrecision n _ | n < 0 = Nothing
reifyPrecision 0 f = Just (f (PProxy :: PProxy One))
reifyPrecision n f = reifyPrecision (n - 1) (f <<< liftTenTimes) where
  liftTenTimes :: forall precision. PProxy precision -> PProxy (TenTimes precision)
  liftTenTimes _ = PProxy

-- | A fixed point representation of a real number, with the specified precision.
-- |
-- | A value is multiplied by the precision, truncated and stored as a big
-- | integer. That is, we approximate the number by numerator/10^precision, storing
-- | only the numerator, and carrying the precision around as type information.
-- |
-- | The `Semiring` and associated instances allow us to perform basic arithmetic
-- | operations.
newtype Fixed (precision :: Precision) = Fixed BigInt.BigInt

-- | Extract the numerator from the representation of the number as a fraction.
-- |
-- | ```
-- | > map numerator (fromNumber 0.1234 :: Fixed P1000)
-- | (Just fromString "123")
-- |
-- | > map numerator (fromNumber 0.1239 :: Fixed P1000)
-- | (Just fromString "123")
-- | ```
numerator :: forall precision. Fixed precision -> BigInt.BigInt
numerator (Fixed n) = n

denominator :: forall precision. KnownPrecision precision => Fixed precision -> Int
denominator _ = reflectPrecision (PProxy :: PProxy precision)

-- | Create a `Fixed` representation of an `Int`.
fromInt
  :: forall precision
   . KnownPrecision precision
  => Int
  -> Fixed precision
fromInt i = Fixed (BigInt.fromInt i * BigInt.fromInt (reflectPrecision (PProxy :: PProxy precision)))

-- | Approximate a `Number` as a `Fixed` value with the specified precision.
-- |
-- | ```
-- | > fromNumber 0.1234 :: Maybe (Fixed P10000)
-- | (Just (fromNumber 0.1234 :: P10000))
-- |
-- | > fromNumber 0.1234 :: Maybe (Fixed P100)
-- | (Just (fromNumber 0.12 :: P100))
-- | ```
-- |
-- | When given a finite `Number`, this function always succeeds: the number is
-- | truncated (rounded towards zero) to the closest possible `Fixed` value.
-- | This function only returns `Nothing` if it is given NaN, or positive or
-- | negative infinity.
-- |
-- | ```
-- | > fromNumber (1.0 / 0.0) :: Maybe (Fixed P100)
-- | Nothing
-- | ```
fromNumber
  :: forall precision
   . KnownPrecision precision
  => Number
  -> Maybe (Fixed precision)
fromNumber n = map Fixed (BigInt.fromNumber (n * Int.toNumber (reflectPrecision (PProxy :: PProxy precision))))

-- | Convert a `Fixed` value to a `Number`.
-- |
-- | _Note_: Overflow is possible here if the numerator is sufficiently large.
-- | Consider using `toString` instead.
toNumber
  :: forall precision
   . KnownPrecision precision
  => Fixed precision
  -> Number
toNumber f = BigInt.toNumber (numerator f) / Int.toNumber (denominator f)

-- | Calculate the largest whole number smaller than or equal to the provided
-- | value.
-- |
-- | ```
-- | > map floor $ fromNumber 0.1 :: Maybe (Fixed P10)
-- | (Just (fromNumber 0.0 :: P10))
-- |
-- | > map floor $ fromNumber 1.0 :: Maybe (Fixed P10)
-- | (Just (fromNumber 1.0 :: P10))
-- |
-- | > floor $ fromNumber (-0.1) :: Maybe (Fixed P10)
-- | (Just (fromNumber (-1.0) :: P10))
-- | ```
floor
  :: forall precision
   . KnownPrecision precision
  => Fixed precision
  -> Fixed precision
floor n = Fixed (numerator n - x) where
  d = BigInt.fromInt (denominator n)
  m = numerator n `mod` d
  x | m < zero = m + d
    | otherwise = m

-- | Calculate the smallest whole number greater than or equal to the provided
-- | value.
-- |
-- | ```
-- | > map ceil $ fromNumber 0.1 :: Maybe (Fixed P10)
-- | (Just (fromNumber 1.0 :: P10))
-- |
-- | > map ceil $ fromNumber 1.0 :: Maybe (Fixed P10)
-- | (Just (fromNumber 1.0 :: P10))
-- |
-- | > map ceil $ fromNumber (-0.1) :: Maybe (Fixed P10)
-- | (Just (fromNumber 0.0 :: P10))
-- | ```
ceil
  :: forall precision
   . KnownPrecision precision
  => Fixed precision
  -> Fixed precision
ceil n = Fixed (numerator n + x) where
  d = BigInt.fromInt (denominator n)
  m = numerator n `mod` d
  x | m == zero = zero
    | m < zero = -m
    | otherwise = d - m

-- | Round the specified value to the nearest whole number.
-- |
-- | ```
-- | > map round $ fromNumber 0.1 :: Maybe (Fixed P10)
-- | (Just (fromNumber 0.0 :: P10))
-- |
-- | > map round $ fromNumber 0.9 :: Maybe (Fixed P10)
-- | (Just (fromNumber 1.0 :: P10))
-- |
-- | > map round $ fromNumber 0.5 :: Maybe (Fixed P10)
-- | (Just (fromNumber 1.0 :: P10))
-- |
-- | > map round $ fromNumber (-0.1) :: Maybe (Fixed P10)
-- | (Just (fromNumber 0.0 :: P10))
-- | ```
round
  :: forall precision
   . KnownPrecision precision
  => Fixed precision
  -> Fixed precision
round n = Fixed (numerator n + x) where
  d = BigInt.fromInt (denominator n)
  m = numerator n `mod` d
  x | m < zero && (m + d) * BigInt.fromInt 2 >= d = -m
    | m * BigInt.fromInt 2 >= d = d - m
    | otherwise = -m

-- | Approximate division of fixed-precision numbers.
-- |
-- | ```
-- | > lift2 approxDiv (fromNumber 22.0) (fromNumber 7.0) :: Maybe (Fixed P100)
-- | (Just (fromNumber 3.14 :: P100))
-- | ```
-- |
-- | _Note_: `Fixed` is not a `EuclideanRing` in general - it is not even
-- | an integral domain, since it has non-zero zero-divisors:
-- |
-- | ```
-- | > lift2 (*) (fromNumber 0.1) (fromNumber 0.1) :: Maybe (Fixed P10)
-- | (Just (fromNumber 0.0 :: P10))
-- | ```
approxDiv
  :: forall precision
   . KnownPrecision precision
  => Fixed precision
  -> Fixed precision
  -> Fixed precision
approxDiv a b = Fixed (x * n / y)
  where
    x = numerator a
    y = numerator b
    n = BigInt.fromInt (denominator a)

-- | Parse a fixed-precision number from a string. Any decimal digits which are
-- | not representable in the specified precision will be ignored.
-- |
-- | ```
-- | > fromString "123.456" :: Maybe (Fixed P1000)
-- | (Just (fromString "123.456" :: P1000))
-- | ```
-- |
-- | Where possible, this function should be preferred over `fromNumber`, since
-- | it is exact (whereas `fromNumber` can only provide an approximation for
-- | larger inputs).
-- |
-- | ```
-- | > fromString "9007199254740992.5" :: Maybe (Fixed P10)
-- | (Just (fromString "9007199254740992.5" :: P10))
-- |
-- | > fromNumber 9007199254740992.5 :: Maybe (Fixed P10)
-- | (Just (fromString "9007199254740992.0" :: P10))
-- | ```
-- |
fromString
  :: forall precision
   . KnownPrecision precision
  => String
  -> Maybe (Fixed precision)
fromString str =
  let
    numDigits = reflectPrecisionDecimalPlaces (PProxy :: PProxy precision)
    denom = BigInt.fromInt (reflectPrecision (PProxy :: PProxy precision))

    isDigit = between '0' '9'
    wholeDigits = StringCU.countPrefix isDigit str
    { before, after } = StringCU.splitAt wholeDigits str

  in do
    wholePart <-
      BigInt.fromString before
    fractionPart <-
      case after of
        "" -> pure zero
        "." -> pure zero
        _ -> do
          guard (StringCU.charAt 0 after == Just '.')
          let raw = StringCU.drop 1 after
          BigInt.fromString (rightJustify numDigits '0' raw)
    pure (Fixed (wholePart * denom + fractionPart))

-- | Represent a `Fixed` value as a string, with the given number of decimal
-- | places.
-- |
-- | ```
-- | > map (toStringWithPrecision 2) (fromString "1234.567" :: Maybe (Fixed P1000))
-- | (Just "1234.56")
-- | ```
-- |
-- | If more decimal places are asked for than the type can provide, the extra
-- | decimal places will be provided as zeroes.
-- |
-- | ```
-- | > map (toStringWithPrecision 3) (fromString "1234.5" :: Maybe (Fixed P10))
-- | (Just "1234.500")
-- | ```
toStringWithPrecision
  :: forall precision
   . KnownPrecision precision
  => Int
  -> Fixed precision
  -> String
toStringWithPrecision requestedDigits fixed@(Fixed n) =
  let
    denom = BigInt.fromInt (reflectPrecision (PProxy :: PProxy precision))
    denomDigits = reflectPrecisionDecimalPlaces (PProxy :: PProxy precision)
    wholePart = n / denom
    fractionalPart = n `mod` denom
  in
    BigInt.toString wholePart
    <> Monoid.guard (requestedDigits > 0)
        ("." <>
          rightJustify requestedDigits '0'
            (leftJustify denomDigits '0' (BigInt.toString fractionalPart)))

-- | Represent a `Fixed` value as a string, using all of the decimal places it
-- | can represent (based on its precision).
-- |
-- | ```
-- | > map toString (fromString "100.5" :: Maybe (Fixed P10))
-- | (Just "100.5")
-- |
-- | > map toString (fromString "100.5" :: Maybe (Fixed P100))
-- | (Just "100.50")
-- | ```
toString
  :: forall precision
   . KnownPrecision precision
  => Fixed precision
  -> String
toString =
  toStringWithPrecision
    (reflectPrecisionDecimalPlaces (PProxy :: PProxy precision))

-- | If a string has less than the given number of characters (measured in code
-- | units), extend it with the given character until it reaches that length.
-- | If it has more, drop them from the end.
rightJustify :: Int -> Char -> String -> String
rightJustify desiredLength padding str =
  let
    actualLength = StringCU.length str
  in
    if actualLength >= desiredLength
      then StringCU.take desiredLength str
      else str <> StringCU.fromCharArray (replicate (desiredLength - actualLength) padding)

-- | If a string has less than the given number of characters (measured in code
-- | units), prepend it with the given character until it reaches that length.
-- | If it has more, drop them from the start.
leftJustify :: Int -> Char -> String -> String
leftJustify desiredLength padding str =
  let
    actualLength = StringCU.length str
  in
    if actualLength >= desiredLength
      then StringCU.takeRight desiredLength str
      else StringCU.fromCharArray (replicate (desiredLength - actualLength) padding) <> str

instance showFixed :: KnownPrecision precision => Show (Fixed precision) where
  show n =
    "(fromString "
      <> show (toString n)
      <> " :: P"
      <> show (reflectPrecision (PProxy :: PProxy precision))
      <> ")"

instance eqFixed :: Eq (Fixed precision) where
  eq (Fixed n) (Fixed m) = eq n m

instance ordFixed :: Ord (Fixed precision) where
  compare (Fixed n) (Fixed m) = compare n m

instance semiringFixed :: KnownPrecision precision => Semiring (Fixed precision) where
  zero = Fixed zero
  add (Fixed n) (Fixed m) = Fixed (n + m)
  one = Fixed (BigInt.fromInt (reflectPrecision (PProxy :: PProxy precision)))
  mul a b = Fixed (numerator a * numerator b / BigInt.fromInt (denominator a))

instance ringFixed :: KnownPrecision precision => Ring (Fixed precision) where
  sub (Fixed n) (Fixed m) = Fixed (n - m)

instance commutativeRingFixed :: KnownPrecision precision => CommutativeRing (Fixed precision)
