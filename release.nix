let ob = import ./example/.obelisk/impl {};
    pkgs = ob.reflex-platform.nixpkgs;
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
in (import (srcNoSymlinks + "/example") {}).exe
