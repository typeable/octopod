(import ./default.nix { }).shellFor {
  tools = {
    cabal = "3.2.0.0";
    hlint = "latest";
    haskell-language-server = "latest";
    ghcid = "latest";
  };

  withHoogle = true;
  exactDeps = true;
}
