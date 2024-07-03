{ pkgs, ... }:

pkgs.rustPlatform.buildRustPackage rec {
  pname = "swim";
  version = "v0.8.0";
  cargoLock.lockFile = "${src}/Cargo.lock";
  src = pkgs.fetchFromGitLab {
    owner = "spade-lang";
    repo = "swim";
    rev = version;
  };
}
