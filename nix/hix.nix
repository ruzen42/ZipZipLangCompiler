{pkgs, ...}: {
  name = "zipZipLangCompiler";
  compiler-nix-name = "ghc926"; 

  crossPlatforms = p: pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([
    p.mingwW64
  ] ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
    p.musl64
  ]);

  shell.tools.cabal = "latest";
}
