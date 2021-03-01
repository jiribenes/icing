{ sources ? import ./nix/sources.nix, nixpkgs ? sources.nixpkgs, compiler ? "ghc8104" }:
let
  pkgs = import nixpkgs { inherit config; };

  # use gitignore to scout which files are important and which aren't
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  # Opendata requires C, C++, Python3 and 'diff'
  opendataDependencies = with pkgs; [ swiProlog ];

  # Function adding executable (runtime) dependencies for a given cabal project
  addDependencies = drv: xs: pkgs.haskell.lib.overrideCabal drv (drv: {
    executableSystemDepends = (drv.executableSystemDepends or [ ]) ++ xs;
    buildDepends = (drv.buildDepends or [ ]) ++ xs;
  });

  config = {
    allowBroken = true; # TODO: fix this
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              # our package:  
              icing-server = addDependencies (haskellPackagesNew.callCabal2nix "icing-server" (gitIgnore ./.) { }) opendataDependencies;
            };
          };
        };
      };
    };
  };
  myHaskellPackages = pkgs.haskell.packages.${compiler};

  # development nix-shell
  shell = with myHaskellPackages; shellFor {
    packages = p: [
      p.icing-server
    ];

    buildInputs = [
      # !!! Add developer tools here: !!!
      # cabal
      cabal-install
      cabal2nix

      # dhall (config language)
      pkgs.dhall

      # IDE-related
      ghcid
      haskell-language-server
      hlint
      stan

      # formatters
      brittany
      pkgs.nixpkgs-fmt

      # Niv for pinning dependencies
      pkgs.niv
      # (import sources.niv { }).niv
    ] ++ opendataDependencies;

    # enable hoogle
    withHoogle = true;
  };

  # static executable
  static-exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages.icing-server);
in
{
  inherit shell static-exe;

  # non-static build
  icing-server = myHaskellPackages.icing-server;
}
