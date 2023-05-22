{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-22.11;
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        ghcVersion = "90";
        ghcAndPkgs = pkgs.haskellPackages.ghcWithPackages (ps: [ps.hakyll ps.witch]);
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            ghcAndPkgs
            cabal-install
            haskell-language-server
          ];
        };
      }
    );
}
