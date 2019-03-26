{ compiler ? "ghc863", pkgs ? import <nixpkgs> {} }:

let

  haskellPackages = pkgs.haskell.packages.${compiler}.extend ( self: super: {
  });

  runtime = haskellPackages.callCabal2nix "aws-lambda-haskell-runtime" ./. {};

in
{
  aws-lambda-haskell-runtime = runtime;
  aws-lambda-haskell-runtime-shell = haskellPackages.shellFor {
    packages = p: [runtime];
    buildInputs = with pkgs; [
      cabal-install
      hlint
      stack
      haskellPackages.ghcid
      haskellPackages.weeder
      zlib
    ];
  };
}
