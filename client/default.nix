{ sources ? import ./nix/sources.nix, nixpkgs ? sources.nixpkgs }:
let
  # load packages with overrides (called overlays in nix), see them below
  pkgs = import nixpkgs { overlays = [ overlay ]; };

  # override the nodejs, yarn versions here
  overlay = self: super:
    {
      nodejs = super.nodejs-12_x;
      yarn = super.yarn.override {
        nodejs = super.nodejs-12_x;
      };
    };

  # create yarn package 'icing-client'  
  icing-client = with pkgs; mkYarnPackage {
    name = "icing-client";
    src = ./.;
    packageJSON = ./package.json;
    yarnLock = ./yarn.lock;

    # installation = build our website into the provided output folder
    # - add any commands used for building here      
    installPhase = ''
      yarn --offline build -o $out
    '';

    # distribution = delete temporary .yarnrc file, do not change this
    # - add any commands used for cleaning the environment here
    distPhase = ''
      rm -f .yarnrc
    '';
  };

  # create a nix development shell for the frontend
  shell = with pkgs; mkShell {
    name = "icing-client-shell";

    # tools available in the shell
    buildInputs = [
      # !!! add new development software here !!! 

      # JavaScript related
      nodejs
      yarn

      # Nix related 
      yarn2nix
      nixpkgs-fmt
      niv
    ];
  };
in
{
  # make 'icing-client' and 'shell' available
  inherit icing-client shell;
}
