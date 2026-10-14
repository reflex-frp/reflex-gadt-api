let systems = [ "x86_64-linux" ];
in builtins.listToAttrs (map (system: {
  name = system;
  value = let
    app = import ./example { inherit system; };
  in {
    example = app.haskell-nix.serverExe.wasm;
    package = app.nixpkgs.packages.reflex-gadt-api;
    recurseForDerivations = true;
  };
}) systems)
