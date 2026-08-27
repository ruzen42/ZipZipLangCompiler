{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        hpkgs = pkgs.haskell.packages.native-bignum.ghc9103;

        zzc = hpkgs.callCabal2nix "zzc" ./.;
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            hpkgs.ghc
            cabal-install
            stack
            git
            zlib
            pkg-config
          ];

          shellHook = ''
            export LD_LIBRARY_PATH="${pkgs.liboqs}/lib:$LD_LIBRARY_PATH"
          '';
        };

        packages.default = zzc;
      });
}
