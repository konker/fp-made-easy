{ name = "fp-made-easy"
, dependencies =
  [ "console", "effect", "lists", "maybe", "prelude", "psci-support", "tuples" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
