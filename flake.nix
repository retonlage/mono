{
  description = "personal monorepo";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/23.05";
    dream2nix.url = "github:nix-community/dream2nix";
    dream2nix.inputs.nixpkgs.follows = "nixpkgs";
    fenix.url = "github:nix-community/fenix/monthly";
    utils.url = "github:numtide/flake-utils";
    lean4.url = "github:leanprover/lean4/v4.2.0-rc1";
  };

  outputs = {nixpkgs, dream2nix, fenix, utils, lean4, ...}:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in
        {
          packages.time = dream2nix;
          packages.autodiff = pkgs.haskellPackages.developPackage {
            root = ./autodiff;
          };
          packages.download-anna = pkgs.stdenv.mkDerivation {
            name = "download-anna";
            src = ./annas-archive-downloader;
            installPhase = ''
              mkdir -p $out/bin
              cp -r . $out/bin
            '';
          };
          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              # rust
              fenix.packages.${system}.rust-analyzer
              (fenix.packages.${system}.default.withComponents [
                "cargo" "clippy" "rustc" "rustfmt"
              ])

              # haskell
              haskellPackages.cabal-install
              (haskell-language-server.override { supportedGhcVersions = [ "945" ]; })

              # i'm prooooooooooving
              (agda.withPackages [
                pkgs.agdaPackages.standard-library
              ])
              # (lean4.defaultPackage.${system})
              z3

              racket

              (python310.withPackages (ps: with ps;[
                amaranth
                requests
                beautifulsoup4
              ]))
            ];
          };
        }
    );
}
