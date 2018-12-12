{ nixpkgs    ? import <nixpkgs> {}
, compiler   ? "ghc844"
, haddock    ? true
, test       ? true
, benchmarks ? false
}:
with builtins;
let
  lib         = nixpkgs.haskell.lib;

  /* The byron-adapter package needs cardano-sl (the library).
     cardano-sl packages need some special non-hackage dependencies, and
     some custom branches of various on-hackage dependencies.
     ouroboros-network is also a dependency of byron-adapter, and it uses
     QuickCheck 2.12.
     Many of the dependencies of cardano-sl are not compatible with this.
     We also use stm 2.5, which causes similar troubles.
     To make it all work, we give special overrides, and use the cardanopkgs
     overlay to get cardano-sl.
     TODO make this optional, until we can reliably get the cardano-sl overlay
     (no hard-coded path).
  */
  ourOverrides = import ./nix/haskell-overrides.nix { dontCheck = lib.dontCheck; };
  cardanopkgsOverrides = import ../cardano-sl/cardanopkgs.nix;
  /* cannot override{ ... }.override{ ... } apparently... so we manually
     compose the overlays
  */
  overlay = self: super: (ourOverrides self super) // (cardanopkgsOverrides self super);

  ghc         = nixpkgs.haskell.packages.${compiler}.override ({ overrides = overlay; });
  callPackage = ghc.callPackage;

  doHaddock = if haddock
    then lib.doHaddock
    else lib.dontHaddock;
  doTest = if test
    then lib.doCheck
    else lib.dontCheck;
  doBench = if benchmarks
    then lib.doBenchmark
    else nixpkgs.lib.id;

in
  rec {

    ouroboros-network = doHaddock(doTest(doBench(
      callPackage ./pkg.nix {
        inherit nixpkgs typed-transitions;
      })));

    byron-adapter = callPackage ./byron-adapter/default.nix {
      inherit (ghc) cardano-sl;
      inherit ouroboros-network;
    };

    typed-transitions = doHaddock(doTest(doBench(
      callPackage ./typed-transitions/default.nix { inherit nixpkgs; }
    )));

  }
