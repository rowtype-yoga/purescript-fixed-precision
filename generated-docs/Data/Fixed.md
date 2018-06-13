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
> fromNumber 0.1234 :: Fixed P10000
fromNumber 0.1234 :: P100

> fromNumber 0.1234 :: Fixed P100
fromNumber 0.12 :: P100
```

#### `toNumber`

``` purescript
toNumber :: forall precision. KnownPrecision precision => Fixed precision -> Number
```

Convert a `Fixed` value to a `Number`.

_Note_: Overflow is possible here if the numerator is sufficiently large.

#### `numerator`

``` purescript
numerator :: forall precision. Fixed precision -> BigInt
```

Extract the numerator from the representation of the number as a fraction.

```
> numerator (fromNumber 0.1234 :: Fixed P1000)
fromString "123"

> numerator (fromNumber 0.1239 :: Fixed P1000)
fromString "123"
```

#### `floor`

``` purescript
floor :: forall precision. KnownPrecision precision => Fixed precision -> Fixed precision
```

Calculate the largest whole number smaller than or equal to the provided
value.

```
> floor $ fromNumber 0.1 :: Fixed P10
fromNumber 0.0 :: P10

> floor $ fromNumber 1.0 :: Fixed P10
fromNumber 1.0 :: P10

> floor $ fromNumber (-0.1) :: Fixed P10
fromNumber (-1.0) :: P10
```

#### `ceil`

``` purescript
ceil :: forall precision. KnownPrecision precision => Fixed precision -> Fixed precision
```

Calculate the smallest whole number greater than or equal to the provided
value.

```
> ceil $ fromNumber 0.1 :: Fixed P10
fromNumber 1.0 :: P10

> ceil $ fromNumber 1.0 :: Fixed P10
fromNumber 1.0 :: P10

> ceil $ fromNumber (-0.1) :: Fixed P10
fromNumber 0.0 :: P10
```

#### `round`

``` purescript
round :: forall precision. KnownPrecision precision => Fixed precision -> Fixed precision
```

Round the specified value to the nearest whole number.

```
> round $ fromNumber 0.1 :: Fixed P10
fromNumber 0.0 :: P10

> round $ fromNumber 0.9 :: Fixed P10
fromNumber 1.0 :: P10

> round $ fromNumber 0.5 :: Fixed P10
fromNumber 1.0 :: P10

> round $ fromNumber (-0.1) :: Fixed P10
fromNumber 0.0 :: P10
```

#### `approxDiv`

``` purescript
approxDiv :: forall precision. KnownPrecision precision => Fixed precision -> Fixed precision -> Fixed precision
```

Approximate division of fixed-precision numbers.

```
> fromNumber 22.0 `approxDiv` fromNumber 7.0 :: Fixed P100
fromNumber 3.14 :: P100
```

_Note_: `Fixed` is not a `EuclideanRing` in general - it is not even
an integral domain, since it has non-zero zero-divisors:

```
> fromNumber 0.1 * fromNumber 0.1 :: Fixed P10
fromNumber 0.0 :: P10
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

##### Instances
``` purescript
KnownPrecision One
(KnownPrecision p) => KnownPrecision (TenTimes p)
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


