{
  description = "lening's solutions for paiza.jp";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs =
    {
      self,
      nixpkgs,
      rust-overlay,
    }:
    let
      system = "x86_64-linux";
      rust-version = "1.75.0";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          rust-overlay.overlays.default
        ];
      };
      rustToolchain = pkgs.rust-bin.stable."${rust-version}".default.override {
        extensions = [
          "rust-src"
          "rust-analyzer"
          "rustfmt-preview"
          "clippy-preview"
        ];
      };
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          rustToolchain
          pkgs.python315
        ];

        RUST_ANAL_PATH = "${rustToolchain}/bin/rust-analyzer";
        RUST_BACKTRACE = "1";

        shellHook = ''
          echo "=========================================="
          echo "Rust Environment Set up already!"
          echo "=========================================="
        '';
      };
    };
}
