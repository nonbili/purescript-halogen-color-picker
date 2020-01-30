{ name =
    "examples"
, dependencies =
    [ "colors", "debug", "effect", "numbers", "halogen-storybook" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "../src/**/*.purs" ]
}
