{
  description = "pomodoro timer in bricks";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    pre-commit-hooks.url = "github:cachix/git-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    brick = {
      url = "github:jtdaugherty/brick";
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    nix-filter,
    pre-commit-hooks,
    ...
  } @ inputs: let
    pkgsFor = system:
      import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.default
        ];
      };
    src = nix-filter.lib {
      root = ./.;
      include = [
        "src/"
        "app/"
        "assets/"
        "package.yaml"
        "pomonad.cabal"
        "LICENSE"
        "notification.wav"
      ];
    };
  in
    {
      overlays.default = final: prev: rec {
        pomonad = final.haskell.lib.justStaticExecutables (
          final.haskellPackages.pomonad.overrideAttrs (oldAttrs: {
            nativeBuildInputs = with final;
              oldAttrs.nativeBuildInputs ++ [makeWrapper];
            configureFlags = oldAttrs.configureFlags ++ ["--ghc-options=-O2"];
            postInstall =
              (oldAttrs.postInstall or "")
              + ''
                wrapProgram $out/bin/pomonad \
                  --suffix PATH : ${final.lib.makeBinPath [final.alsa-utils]}
              '';
          })
        );

        haskellPackages = prev.haskell.packages.ghc912.override (old:
          with final.haskell.lib; {
            overrides =
              final.lib.composeExtensions
              (old.overrides or (_: _: {}))
              (self: super: {
                brick = self.callCabal2nix "brick" inputs.brick {};
                pomonad = self.callCabal2nix "pomonad" src {};
              });
          });
      };
    }
    // flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = pkgsFor system;
        precommitCheck = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            alejandra.enable = true;
            hlint.enable = true;
            hpack.enable = true;
            ormolu.enable = true;
            ripsecrets.enable = true;
          };
        };
      in rec {
        packages.default = packages.pomonad;
        packages.pomonad = pkgs.pomonad;

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: with p; [pomonad];
          buildInputs = with pkgs;
          with pkgs.haskellPackages;
            [
              alejandra
              cabal-install
              haskell-language-server
              hlint
              nil
              ormolu
              pkgs.helix
              statix
            ]
            ++ packages.pomonad.buildInputs;

          shellHook = ''
            ${precommitCheck.shellHook}
          '';
        };

        checks = {pre-commit-check = precommitCheck;};
      }
    );
  nixConfig = {
    extra-substituters = [
      "https://opensource.cachix.org"
      "https://haskell-language-server.cachix.org"
    ];
    extra-trusted-public-keys = [
      "opensource.cachix.org-1:6t9YnrHI+t4lUilDKP2sNvmFA9LCKdShfrtwPqj2vKc="
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
    ];
  };
}
