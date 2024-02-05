{
  description = "personal monorepo";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/23.05";
    dream2nix.url = "github:nix-community/dream2nix";
    dream2nix.inputs.nixpkgs.follows = "nixpkgs";
    rust-overlay.url = "github:oxalica/rust-overlay";
    utils.url = "github:numtide/flake-utils";
    lean4.url = "github:leanprover/lean4/v4.2.0-rc1";
  };

  outputs = {nixpkgs, dream2nix, rust-overlay, utils, lean4, ...}:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
        {
          nixpkgs.overlays = [
            rust-overlay.overlay
          ];
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
              cargo rustc rust-analyzer

              # haskell
              haskellPackages.cabal-install
              (haskell-language-server.override { supportedGhcVersions = [ "945" ]; })

              # i'm prooooooooooving
              (agda.withPackages [
                pkgs.agdaPackages.standard-library
              ])
              (lean4.defaultPackage.${system})
              z3

              racket

              (python3.withPackages(pp: with pp; [requests beautifulsoup4]))
            ];
          };
        }
    );
}
