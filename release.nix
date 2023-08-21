let systems = [ "x86_64-linux" "x86_64-darwin" ];
    lib = (import ./example/.obelisk/impl {}).nixpkgs.lib;
in lib.genAttrs systems (system: let
    ob = import ./example/.obelisk/impl { inherit system; };
    pkgs = ob.reflex-platform.nixpkgs;
    ghc = ob.reflex-platform.ghc.override {
      overrides = self: super: {
        aeson-qq = self.callHackage "aeson-qq" "0.8.4" {};
      };
    };
    srcNoSymlinks = pkgs.runCommand "deref-src" {} ''
      mkdir $out
      echo $out
      mkdir -p $out/example/common/src
      mkdir -p $out/example/reflex-gadt-api
      cp -r ${./.}/* $out/example/reflex-gadt-api/
      cat ${./Readme.md} > $out/example/common/src/Readme.lhs
      ${pkgs.gnused}/bin/sed \
        's~reflex-gadt-api = ./. + \"/../\"~reflex-gadt-api = ./reflex-gadt-api~g' \
        ${./example/default.nix} > $out/example/default.nix
      mkdir -p $out/example/.obelisk/impl
      cp ${./example/.obelisk/impl/default.nix} $out/example/.obelisk/impl/default.nix
      ${pkgs.rsync}/bin/rsync -arv --exclude='Readme.lhs' --exclude='default.nix' ${./example}/ $out/example/

    '';
  in
    { example = (import (srcNoSymlinks + "/example") {}).exe;
      package = ghc.callCabal2nix "reflex-gadt-api" (builtins.fetchGit ./.) {};
      recurseForDerivations = true;
    })
