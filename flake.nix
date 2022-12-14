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
  nixConfig.extra-substituters = [ "https://slack-web.cachix.org" ];
  nixConfig.extra-trusted-public-keys = [ "slack-web.cachix.org-1:k2R87YfuLXjp6C33D/KZZEBhYWEE9t5ph8oKa/u0ccE=" ];

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

          checks = rec {
            # due to https://github.com/NixOS/nix/issues/4265 breaking
            # import-from-derivation inside nix flake checks, nix flake check
            # does not work and you need to use `nix build .#checks.yourSystem.all`
            all = pkgs.writeTextFile {
              name = "all-checks";
              text = ''
                ${pre-commit-check}
                ${slack-web}
              '';
            };

            inherit (self.packages.${system}) slack-web;

            pre-commit-check = pre-commit-hooks.lib.${system}.run {
              src = ./.;
              tools.fourmolu = pkgs.haskell.packages.${ghcVer}.fourmolu;
              hooks = {
                fourmolu.enable = true;
              };
              settings = {
                # XXX: For bizarre reasons, it seems that the fourmolu pre-commit
                # hook has Fourmolu not read these from the cabal files as it
                # normally would. Seems like a bug...
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

            # 0.6.3 in the repo
            refined = hfinal.refined_0_7;

            pretty-simple = hfinal.callHackageDirect
              {
                pkg = "pretty-simple";
                ver = "4.1.2.0";
                sha256 = "sha256-uM1oyi/isWMicKPIw0KKeRJzY8Zu5yQNE0A2mpeBPHg=";
              }
              { };

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
