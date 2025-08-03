{
  description = "zzcompiler flake";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          inherit (haskellNix) config;
          overlays = [ haskellNix.overlay ];
        };
        project = pkgs.haskell-nix.project' {
          src = ./.;
          compiler-nix-name = "ghc96";
          shell.tools = {
            cabal = {};
            stack = {};
          };
          shell.buildInputs = with pkgs; [
            nixpkgs-fmt
          ];
        };
      in {
        packages.default = project.shell;
        devShells.default = project.shell;
      });
}
