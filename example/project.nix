{ pkgs, obeliskLib, nix-haskell-compilers, ... }:

let lib = (import (import ./inputs.nix).nixpkgs {}).lib;
in
{

  # The nixpkgs driver has no wasm compiler of its own. The ghc-wasm-meta
  # bindist gives it one. The haskell.nix driver builds its own, so the
  # bindist is scoped to the driver that needs it. The series matches that
  # driver's own compiler.
  imports = [
    (import "${nix-haskell-compilers}/ghc-wasm-meta" {
      flavour = "9.12";
      drivers = [ "nixpkgs" ];
    })
  ];

  name = "reflex-gadt-api-example";
  src = ./.;

  source-repository-packages = obeliskLib.source-repository-packages // {
    reflex-gadt-api = obeliskLib.cleanSource {
      src = ../.;
      name = "reflex-gadt-api-src";
      patterns = "/example";
    };
  };

  # Frontend target: the haskell-nix driver defaults to "wasm"; the nixpkgs
  # driver's serverExe.js override switches it to "js".
  obelisk.static.path = import ./static { inherit pkgs; };
  obelisk.config.path = ./config;

  shell = {
    crossPlatforms = ps: with ps; [
      wasi32
      ghcjs
    ];
  };

  nixpkgs.options.overrides = [
    (self: super: {
      aeson-qq = self.callHackage "aeson-qq" "0.8.4" {};
    })
  ];

  nixpkgs.options.packages.reflex-gadt-api.src =
    obeliskLib.cleanSource {
      src = ../.;
      name = "reflex-gadt-api-src";
      patterns = "/example";
    };

}
