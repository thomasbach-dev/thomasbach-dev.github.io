{
  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/0b3d618173114c64ab666f557504d6982665d328";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        ghcAndPkgs = pkgs.haskellPackages.ghcWithPackages
          (ps: [ ps.clay ps.hakyll ps.witch ]);
      in {
        packages = rec {
          thomasbach-dev =
            pkgs.haskellPackages.callPackage (import ./thomasbach-dev.nix) { };
          default = thomasbach-dev;
        };
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [ ghcAndPkgs cabal-install ormolu ];
        };
      });
}
