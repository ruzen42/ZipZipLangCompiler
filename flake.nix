{
  description = "Offline Nix flake for Haskell shell-runner (GHC 9.10.2)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskell = pkgs.haskellPackages;
        ghc = haskell.ghc;  # GHC 9.10.2
        hsDeps = haskell.ghcWithPackages (p: with p; [ servant servant-server warp aeson text bytestring process ]);
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            ghc
            pkgs.cabal-install
            haskell.stack
            pkgs.git
            pkgs.zlib
            pkgs.pkg-config
          ];
        };

        packages.shell-runner = pkgs.haskellPackages.callCabal2nix "shell-runner" ./. {};
      });
}

