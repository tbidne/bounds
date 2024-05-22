{
  description = "Bounded typeclasses";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.nix-hs-utils.url = "github:tbidne/nix-hs-utils";
  outputs =
    inputs@{ flake-parts
    , nix-hs-utils
    , nixpkgs
    , self
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem = { pkgs, ... }:
        let
          ghc-version = "ghc982";
          hlib = pkgs.haskell.lib;
          compiler = pkgs.haskell.packages."${ghc-version}";
          mkPkg = returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit compiler pkgs returnShellEnv;
              name = "bounds";
              root = ./.;
            };
          compilerPkgs = { inherit pkgs compiler; };
        in
        {
          packages.default = mkPkg false;
          devShells.default = mkPkg true;

          apps = {
            format = nix-hs-utils.format compilerPkgs;
            lint = nix-hs-utils.lint compilerPkgs;
            lintRefactor = nix-hs-utils.lintRefactor compilerPkgs;
          };
        };
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
