(import ./default.nix { }).shellFor {
  tools = {
    cabal = "3.2.0.0";
    hlint = "latest";
    haskell-language-server = {
      version = "latest";
      index-state = "2021-07-02T00:00:00Z";
    };
    ghcid = "latest";
  };

  withHoogle = true;
  exactDeps = true;
}
