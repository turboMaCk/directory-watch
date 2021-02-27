let
  unix-recursive-src = fetchTarball "https://github.com/turboMaCk/unix-recursive/archive/df046236b2d6d65daed36068c9e06b8af8f93586.tar.gz";
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages =
        pkgs.haskellPackages.override {
            overrides = self: super: {
              directory-watch = self.callCabal2nix "directory-watch" ../. {};
              unix-recursive = pkgs.haskell.lib.dontCheck (self.callCabal2nix "unix-recursive" unix-recursive-src {});
            };
        };
    };
  };
in
import ./pkgs.nix { inherit config; }
