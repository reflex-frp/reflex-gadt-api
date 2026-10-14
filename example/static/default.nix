{ pkgs ? import (import ../inputs.nix).nixpkgs {} }:

# Static assets derivation.
# Add build steps here (e.g. postcss, tailwind, sass).
pkgs.runCommand "catnet-static" {} ''
  mkdir -p "$out"
  cp -R ${./src}/. "$out/"
''
