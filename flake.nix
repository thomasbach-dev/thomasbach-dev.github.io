{
  description = "my website";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-2105";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          thomasbachDev =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8105";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = {};
                hlint = {};
                haskell-language-server = {};
              };
              # This adds `js-unknown-ghcjs-cabal` to the shell.
              #shell.crossPlatform = p: [p.ghcjs];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; };
      flake = pkgs.thomasbachDev.flake {
        # This adds support for `nix build .#js-unknown-ghcjs-cabal:hello:exe:hello`
        #crossPlatforms = p: [p.ghcjs];
      };
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."thomasbach-dev:exe:thomasbach-dev";
    });
}
