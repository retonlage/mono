{
  description = "personal monorepo";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/23.05";
    dream2nix.url = "github:nix-community/dream2nix";
    dream2nix.inputs.nixpkgs.follows = "nixpkgs";
    rust-overlay.url = "github:oxalica/rust-overlay";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = {nixpkgs, dream2nix, rust-overlay, utils, ...}:
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
          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              # rust
              cargo rustc rust-analyzer

              # haskell
              haskellPackages.cabal-install
              (haskell-language-server.override { supportedGhcVersions = [ "945" ]; })
            ];
          };
        }
    );
}
