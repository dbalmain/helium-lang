{
  description = "Helium language";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs =
    inputs@{
      nixpkgs,
      flake-parts,
      haskell-flake,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;

      imports = [
        haskell-flake.flakeModule
      ];

      perSystem =
        {
          pkgs,
          ...
        }:
        {
          haskellProjects.default = {
            basePackages = pkgs.haskell.packages.ghc984;

            devShell = {
              enable = true;

              tools = hp: {
                inherit (hp)
                  cabal-install
                  haskell-language-server
                  hlint
                  fourmolu
                  ;
              };

              mkShellArgs = {
                nativeBuildInputs = with pkgs; [
                  zlib
                ];
              };
            };
          };
        };
    };
}
