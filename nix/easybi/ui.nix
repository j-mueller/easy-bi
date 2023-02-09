{ npm
, src
, system
, nodejs
}:
derivation {
  inherit npm src system nodejs;

  name    = "easy-bi-ui";
  builder = "${npm}";
  args    = ["run" "build"]
}