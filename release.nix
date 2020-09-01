let
  sources = import ./nix/sources.nix;
in
{ compiler ? "ghc884"
, pkgs ? import <nixpkgs> {} #import sources.nixpkgs { }
}:

let
  inherit (pkgs.lib.trivial) flip pipe;
  inherit (pkgs.haskell.lib) appendPatch appendConfigureFlags;

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hpNew: hpOld: {
      hakyll =
        pipe
           hpOld.hakyll
           [ (flip appendPatch ./hakyll.patch)
             (flip appendConfigureFlags [ "-f" "watchServer" "-f" "previewServer" ])
           ];

      #blog = hpNew.callCabal2nix "blog" ./. { };
      blog = pkgs.haskell.packages.${compiler}.callPackage ({ mkDerivation, base, hakyll, hakyll-sass, stdenv }:
        mkDerivation {
          pname = "blog-hakyll";
          version = "0.1.0.0";
          src = ./.;
          isLibrary = false;
          isExecutable = true;
          executableHaskellDepends = [ base hakyll hakyll-sass ];
          license = "unknown";
          hydraPlatforms = stdenv.lib.platforms.none;
        }
      ) {};

      niv = import sources.niv { };
    };
  };

  project = haskellPackages.blog;
in
{
  project = project;

  shell = haskellPackages.shellFor {
    packages = p: with p; [
      project
    ];
    buildInputs = with haskellPackages; [
      ghcid
      hlint       # or ormolu
      pkgs.niv
      pkgs.cacert # needed for niv
      pkgs.nix    # needed for niv
      (pkgs.texlive.combine { inherit (pkgs.texlive) scheme-medium stackengine tikzmark; })
      pkgs.imagemagick
      pkgs.cabal-install
    ];
    withHoogle = true;
  };
}
