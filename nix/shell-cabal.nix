with (import ./default.nix);
mkShell {
  name = "directory-watch-cabal-shell";
  buildInputs = [ stack fourmolu ];
  inputsFrom = [ haskellPackages.directory-watch.env ];
}
