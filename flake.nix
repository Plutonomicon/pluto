{
  nixConfig.bash-prompt = "[nix-develop-pluto:] ";
  description = "A very basic flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.plutus.url = "github:input-output-hk/plutus";
  inputs.cardano-node.url = "github:input-output-hk/cardano-node";
  inputs.plutus-apps.url = "github:input-output-hk/plutus-apps";
  outputs = { self, nixpkgs, plutus, flake-utils, haskellNix, cardano-node, plutus-apps }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        deferPluginErrors = true;
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            pluto =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc8107";
                projectFileName = "stack.yaml";
                modules = [{
                  packages = {
                    marlowe.flags.defer-plugin-errors = deferPluginErrors;
                    plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;
                    plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;
                    plutus-contract.flags.defer-plugin-errors = deferPluginErrors;
                    cardano-crypto-praos.components.library.pkgconfig =
                      pkgs.lib.mkForce [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
                    cardano-crypto-class.components.library.pkgconfig =
                      pkgs.lib.mkForce [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];

                  };
                }];
                shell.tools = {
                  cabal = { };
                  ghcid = { };
                  hlint = { };
                  haskell-language-server = { };
                  stylish-haskell = { };
                };
                # Non-Haskell shell tools go here
                shell.buildInputs = with pkgs; [
                  nixpkgs-fmt
                  cardano-node.outputs.packages.x86_64-linux."cardano-node:exe:cardano-node"
                  cardano-node.outputs.packages.x86_64-linux."cardano-cli:exe:cardano-cli"
                ];
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.pluto.flake { };
      in flake // {
        defaultPackage = flake.packages."pluto:exe:pluto";
      });
}
