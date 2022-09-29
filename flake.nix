{
  description = "Slack library for Haskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  nixConfig.allow-import-from-derivation = true; # cabal2nix uses IFD

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks }:
    let
      ghcVer = "ghc924";
      makeHaskellOverlay = overlay: final: prev: {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            ${ghcVer} = prev.haskell.packages."${ghcVer}".override (oldArgs: {
              overrides =
                prev.lib.composeExtensions (oldArgs.overrides or (_: _: { }))
                  (overlay prev);
            });
          };
        };
      };

      out = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ self.overlays.default ];
            config.allowBroken = true;
          };

        in
        {
          packages = rec {
            default = slack-web;
            slack-web = pkgs.haskell.packages.${ghcVer}.slack-web;
          };

          checks = {
            # due to https://github.com/NixOS/nix/issues/4265 breaking
            # import-from-derivation inside nix flake checks, this is kind of
            # useless
            # inherit (self.packages.${system}) slack-web;

            pre-commit-check = pre-commit-hooks.lib.${system}.run {
              src = ./.;
              tools.fourmolu = pkgs.haskell.packages.${ghcVer}.fourmolu;
              hooks = {
                fourmolu.enable = true;
              };
              settings = {
                ormolu.defaultExtensions = [
                  "ImportQualifiedPost"
                ];
              };
            };
          };

          # for debugging
          inherit pkgs;

          devShells.default =
            let haskellPackages = pkgs.haskell.packages.${ghcVer};
            in
            haskellPackages.shellFor {
              packages = p: [ self.packages.${system}.slack-web ];
              withHoogle = true;
              buildInputs = with haskellPackages; [
                haskell-language-server
                fourmolu
                # jacked on this particular nixpkgs version
                # ghcid
                cabal-install
                fast-tags
              ] ++ (with pkgs; [
                sqlite
              ]);
              shellHook = self.checks.${system}.pre-commit-check.shellHook;
            };
        };
    in
    flake-utils.lib.eachDefaultSystem out // {
      # this stuff is *not* per-system
      overlays = {
        default = makeHaskellOverlay (prev: hfinal: hprev:
          let hlib = prev.haskell.lib; in
          {
            slack-web = hprev.callCabal2nix "slack-web" ./. { };
            # test-suite doesn't compile; probably fixed in a newer nixpkgs,
            # but there's other jackage on newer nixpkgs such as the ghcid bug:
            # https://github.com/NixOS/nixpkgs/issues/140774#issuecomment-1186546139
            # and the fourmolu/ormolu bug:
            # https://github.com/tweag/ormolu/issues/927
            mutable-containers = hlib.dontCheck hprev.mutable-containers;

            # it's not yet in hackage2nix
            string-variants = hfinal.callHackageDirect
              {
                pkg = "string-variants";
                ver = "0.1.0.1";
                sha256 = "sha256-7oNYwPP8xRNYxKNdNH+21zBHdeUeiWBtKOK5G43xtSQ=";
              }
              { };
          });
      };
    };
}
