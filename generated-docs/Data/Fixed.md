## Module Data.Fixed

This module defines a numeric type `Fixed` for working with
fixed point numbers in base 10. The precision is tracked in
the types.

#### `Fixed`

``` purescript
newtype Fixed (precision :: Precision)
```

A fixed point representation of a real number, with the specified precision.

A value is multiplied by the precision, truncated and stored as a big
integer. That is, we approximate the number by numerator/10^precision, storing
only the numerator, and carrying the precision around as type information.

The `Semiring` and associated instances allow us to perform basic arithmetic
operations.

##### Instances
``` purescript
(KnownPrecision precision) => Show (Fixed precision)
Eq (Fixed precision)
Ord (Fixed precision)
(KnownPrecision precision) => Semiring (Fixed precision)
(KnownPrecision precision) => Ring (Fixed precision)
(KnownPrecision precision) => CommutativeRing (Fixed precision)
```

#### `fromInt`

``` purescript
fromInt :: forall precision. KnownPrecision precision => Int -> Fixed precision
```

Create a `Fixed` representation of an `Int`.

#### `fromNumber`

``` purescript
fromNumber :: forall precision. KnownPrecision precision => Number -> Maybe (Fixed precision)
```

Approximate a `Number` as a `Fixed` value with the specified precision.

```
> fromNumber 0.1234 :: Maybe (Fixed P10000)
(Just (fromNumber 0.1234 :: P10000))

> fromNumber 0.1234 :: Maybe (Fixed P100)
(Just (fromNumber 0.12 :: P100))
```

When given a finite `Number`, this function always succeeds: the number is
truncated (rounded towards zero) to the closest possible `Fixed` value.
This function only returns `Nothing` if it is given NaN, or positive or
negative infinity.

```
> fromNumber (1.0 / 0.0) :: Maybe (Fixed P100)
Nothing
```

#### `toNumber`

``` purescript
toNumber :: forall precision. KnownPrecision precision => Fixed precision -> Number
```

Convert a `Fixed` value to a `Number`.

_Note_: Overflow is possible here if the numerator is sufficiently large.
Consider using `toString` instead.

#### `fromString`

``` purescript
fromString :: forall precision. KnownPrecision precision => String -> Maybe (Fixed precision)
```

Parse a fixed-precision number from a string. Any decimal digits which are
not representable in the specified precision will be ignored.

```
> fromString "123.456" :: Maybe (Fixed P1000)
(Just (fromString "123.456" :: P1000))
```

Where possible, this function should be preferred over `fromNumber`, since
it is exact (whereas `fromNumber` can only provide an approximation for
larger inputs).

```
> fromString "9007199254740992.5" :: Maybe (Fixed P10)
(Just (fromString "9007199254740992.5" :: P10))

> fromNumber 9007199254740992.5 :: Maybe (Fixed P10)
(Just (fromString "9007199254740992.0" :: P10))
```


#### `toString`

``` purescript
toString :: forall precision. KnownPrecision precision => Fixed precision -> String
```

Represent a `Fixed` value as a string, using all of the decimal places it
can represent (based on its precision).

```
> map toString (fromString "100.5" :: Maybe (Fixed P10))
(Just "100.5")

> map toString (fromString "100.5" :: Maybe (Fixed P100))
(Just "100.50")
```

#### `toStringWithPrecision`

``` purescript
toStringWithPrecision :: forall precision. KnownPrecision precision => Int -> Fixed precision -> String
```

Represent a `Fixed` value as a string, with the given number of decimal
places.

```
> map (toStringWithPrecision 2) (fromString "1234.567" :: Maybe (Fixed P1000))
(Just "1234.56")
```

If more decimal places are asked for than the type can provide, the extra
decimal places will be provided as zeroes.

```
> map (toStringWithPrecision 3) (fromString "1234.5" :: Maybe (Fixed P10))
(Just "1234.500")
```

#### `numerator`

``` purescript
numerator :: forall precision. Fixed precision -> BigInt
```

Extract the numerator from the representation of the number as a fraction.

```
> map numerator (fromNumber 0.1234 :: Fixed P1000)
(Just fromString "123")

> map numerator (fromNumber 0.1239 :: Fixed P1000)
(Just fromString "123")
```

#### `floor`

``` purescript
floor :: forall precision. KnownPrecision precision => Fixed precision -> Fixed precision
```

Calculate the largest whole number smaller than or equal to the provided
value.

```
> map floor $ fromNumber 0.1 :: Maybe (Fixed P10)
(Just (fromNumber 0.0 :: P10))

> map floor $ fromNumber 1.0 :: Maybe (Fixed P10)
(Just (fromNumber 1.0 :: P10))

> floor $ fromNumber (-0.1) :: Maybe (Fixed P10)
(Just (fromNumber (-1.0) :: P10))
```

#### `ceil`

``` purescript
ceil :: forall precision. KnownPrecision precision => Fixed precision -> Fixed precision
```

Calculate the smallest whole number greater than or equal to the provided
value.

```
> map ceil $ fromNumber 0.1 :: Maybe (Fixed P10)
(Just (fromNumber 1.0 :: P10))

> map ceil $ fromNumber 1.0 :: Maybe (Fixed P10)
(Just (fromNumber 1.0 :: P10))

> map ceil $ fromNumber (-0.1) :: Maybe (Fixed P10)
(Just (fromNumber 0.0 :: P10))
```

#### `round`

``` purescript
round :: forall precision. KnownPrecision precision => Fixed precision -> Fixed precision
```

Round the specified value to the nearest whole number.

```
> map round $ fromNumber 0.1 :: Maybe (Fixed P10)
(Just (fromNumber 0.0 :: P10))

> map round $ fromNumber 0.9 :: Maybe (Fixed P10)
(Just (fromNumber 1.0 :: P10))

> map round $ fromNumber 0.5 :: Maybe (Fixed P10)
(Just (fromNumber 1.0 :: P10))

> map round $ fromNumber (-0.1) :: Maybe (Fixed P10)
(Just (fromNumber 0.0 :: P10))
```

#### `approxDiv`

``` purescript
approxDiv :: forall precision. KnownPrecision precision => Fixed precision -> Fixed precision -> Fixed precision
```

Approximate division of fixed-precision numbers.

```
> lift2 approxDiv (fromNumber 22.0) (fromNumber 7.0) :: Maybe (Fixed P100)
(Just (fromNumber 3.14 :: P100))
```

_Note_: `Fixed` is not a `EuclideanRing` in general - it is not even
an integral domain, since it has non-zero zero-divisors:

```
> lift2 (*) (fromNumber 0.1) (fromNumber 0.1) :: Maybe (Fixed P10)
(Just (fromNumber 0.0 :: P10))
```

#### `Precision`

``` purescript
kind Precision
```

A kind for type-level precision information

#### `One`

``` purescript
data One :: Precision
```

No decimal places

##### Instances
``` purescript
KnownPrecision One
```

#### `TenTimes`

``` purescript
data TenTimes :: Precision -> Precision
```

One more decimal place

##### Instances
``` purescript
(KnownPrecision p) => KnownPrecision (TenTimes p)
```

#### `P1`

``` purescript
type P1 = One
```

#### `P10`

``` purescript
type P10 = TenTimes P1
```

One decimal place

#### `P100`

``` purescript
type P100 = TenTimes P10
```

Two decimal places

#### `P1000`

``` purescript
type P1000 = TenTimes P100
```

Three decimal places

#### `P10000`

``` purescript
type P10000 = TenTimes P1000
```

Four decimal places

#### `P100000`

``` purescript
type P100000 = TenTimes P10000
```

Five decimal places

#### `P1000000`

``` purescript
type P1000000 = TenTimes P100000
```

Six decimal places

#### `PProxy`

``` purescript
data PProxy (precision :: Precision)
  = PProxy
```

A value-level proxy for a type-level precision.

#### `KnownPrecision`

``` purescript
class KnownPrecision (precision :: Precision)  where
  reflectPrecision :: PProxy precision -> Int
```

Precision which is known, i.e. it can be reflected to a
value at runtime, given a `PProxy`.

`reflectPrecision` returns a multiple of ten, corresponding
to the maximum number of decimal places which can be stored.

```
> reflectPrecision (PProxy :: PProxy P1000)
1000
```

##### Instances
``` purescript
KnownPrecision One
(KnownPrecision p) => KnownPrecision (TenTimes p)
```

#### `reflectPrecisionDecimalPlaces`

``` purescript
reflectPrecisionDecimalPlaces :: forall precision. KnownPrecision precision => PProxy precision -> Int
```

Get the number of decimal places associated with a given `Precision` at
the value level.

```
> reflectPrecisionDecimalPlaces (PProxy :: PProxy P1000)
3
```

#### `reifyPrecision`

``` purescript
reifyPrecision :: forall r. Int -> (forall precision. KnownPrecision precision => PProxy precision -> r) -> Maybe r
```

Reify an non-negative integer (a power of ten) as a `Precision`.

For example

```
> reifyPrecision 0 reflectPrecision
Just 1
> reifyPrecision 1 reflectPrecision
Just 10
> reifyPrecision 2 reflectPrecision
Just 100
> reifyPrecision (-1) reflectPrecision
Nothing
```


