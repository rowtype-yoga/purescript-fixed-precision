{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "fixed-precision"
, dependencies = [ "integers", "maybe", "bigints", "strings", "math" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "Apache-2.0"
, repository = "https://github.com/lumihq/purescript-fixed-precision"
}
