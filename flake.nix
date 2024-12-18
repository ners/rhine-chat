{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    rhine = {
      url = "github:turion/rhine";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    matrix-client = {
      url = "github:ners/matrix-client-haskell";
      flake = false;
    };
    vodozemac = {
      url = "github:ners/vodozemac-haskell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      sourceFilter = root: with lib.fileset; toSource {
        inherit root;
        fileset = fileFilter (file: any file.hasExt [ "cabal" "hs" "hsc" "md" ]) root;
      };
      readDirs = root: attrNames (lib.filterAttrs (_: type: type == "directory") (readDir root));
      readFiles = root: attrNames (lib.filterAttrs (_: type: type == "regular") (readDir root));
      basename = path: suffix: with lib; pipe path [
        (splitString "/")
        last
        (removeSuffix suffix)
      ];
      cabalProjectPackages = root: with lib; foreach (readDirs root) (dir:
        let
          path = "${root}/${dir}";
          files = readFiles path;
          cabalFiles = filter (strings.hasSuffix ".cabal") files;
          pnames = map (path: basename path ".cabal") cabalFiles;
          pname = if pnames == [ ] then null else head pnames;
        in
        optionalAttrs (pname != null) { ${pname} = path; }
      );
      cabalProjectPnames = root: lib.attrNames (cabalProjectPackages root);
      cabalProjectOverlay = root: hfinal: hprev: with lib;
        mapAttrs
          (pname: path: hfinal.callCabal2nix pname path { })
          (cabalProjectPackages root);
      project = sourceFilter ./.;
      pnames = cabalProjectPnames project;
      ghcsFor = pkgs: with lib; foldlAttrs
        (acc: name: hp:
          let
            version = getVersion hp.ghc;
            majorMinor = versions.majorMinor version;
            ghcName = "ghc${replaceStrings ["."] [""] majorMinor}";
            newAcc = acc // { ${ghcName} = hp; };
          in
          if ! hp ? ghc || hp ? ${ghcName} || versionOlder version "9.6" || versionAtLeast version "9.10"
          then acc
          else newAcc
        )
        { }
        pkgs.haskell.packages;
      hpsFor = pkgs: { default = pkgs.haskellPackages; } // ghcsFor pkgs;
      aesonOverlay = lib.composeManyExtensions [
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions [
              prev.haskell.packageOverrides
              (hfinal: hprev: with prev.haskell.lib.compose; {
                aeson = doJailbreak hprev.aeson_2_2_3_0;
                attoparsec-aeson = doJailbreak hprev.attoparsec-aeson_2_2_2_0;
                matrix-client = hfinal.callCabal2nix "matrix-client" "${inputs.matrix-client}/matrix-client" { };
                rhine-matrix = hfinal.callCabal2nix "rhine-matrix" ./rhine-matrix { };
              })
            ];
          };
        })
      ];
      overlay = lib.composeManyExtensions [
        inputs.rhine.overlays.default
        inputs.vodozemac.overlays.default
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions [
              prev.haskell.packageOverrides
              (cabalProjectOverlay project)
              (hfinal: hprev: 
                let
                  aesonPrev = prev.extend aesonOverlay;
                  aesonHprev = aesonPrev.haskell.packages."ghc${replaceStrings ["."] [""] prev.ghc.version}";
                in
                {
                  inherit (aesonHprev) rhine-matrix; 
                }
              )
            ];
          };
        })
      ];

      synapseConfig = { modulesPath, config, pkgs, ... }:
        let
          dendritePort = 8009;
          synapsePort = 8008;
          synapseHost = "http://${config.networking.fqdn}:${toString synapsePort}";
          synapseSharedSecret = "foo";
        in
        {
          imports = [
            "${modulesPath}/virtualisation/qemu-vm.nix"
          ];
          config = {
            networking = {
              hostName = "synapse";
              domain = "test";
            };
            virtualisation = {
              graphics = false;
              diskSize = 10000;
              forwardPorts = [
                { from = "host"; host.port = 2222; guest.port = 22; }
                { from = "host"; host.port = synapsePort; guest.port = synapsePort; }
                { from = "host"; host.port = dendritePort; guest.port = dendritePort; }
              ];
            };
            services.openssh = {
              enable = true;
              permitRootLogin = "yes";
            };
            users.mutableUsers = false;
            users.defaultUserShell = pkgs.fish;
            users.extraUsers.root.password = "root";
            networking.firewall.enable = false;

            environment.systemPackages = with pkgs; [
              dendrite
              matrix-synapse
              matrix-commander
            ];

            nix.settings.experimental-features = [ "nix-command" "flakes" ];

            programs.neovim = {
              enable = true;
              defaultEditor = true;
              viAlias = true;
              vimAlias = true;
            };

            programs.fish.enable = true;
            programs.starship.enable = true;

            services.matrix-synapse = {
              enable = true;
              settings = {
                server_name = config.networking.fqdn;
                database.name = "sqlite3";
                listeners = [{
                  port = synapsePort;
                  bind_addresses = [ "0.0.0.0" ];
                  type = "http";
                  tls = false;
                  x_forwarded = true;
                  resources = [
                    {
                      names = [ "client" ];
                      compress = true;
                    }
                    {
                      names = [ "federation" ];
                      compress = false;
                    }
                  ];
                }];
              };
              extraConfigFiles = [
                (pkgs.writeTextFile {
                  name = "registration_secret.yml";
                  text = ''
                    registration_shared_secret: "${synapseSharedSecret}"
                  '';
                })
              ];
            };

            services.getty.autologinUser = "root";

            systemd.services.initSynapse = {
              wantedBy = [ "multi-user.target" ];
              requires = [ "matrix-synapse.service" ];
              after = [ "matrix-synapse.service" ];
              serviceConfig = {
                Type = "oneshot";
                WorkingDirectory = "/root";
              };
              path = config.environment.systemPackages;
              script = ''
                register_new_matrix_user \
                  --admin \
                  -u admin \
                  -p admin123 \
                  -k ${synapseSharedSecret} \
                  ${synapseHost} \
                  || true

                register_new_matrix_user \
                  --no-admin \
                  -u bot \
                  -p bot123 \
                  -k ${synapseSharedSecret} \
                  ${synapseHost} \
                  || true

                matrix-commander \
                  --login password \
                  --homeserver ${synapseHost} \
                  --user-login admin \
                  --password admin123 \
                  --room-default lounge \
                  --device matrix-commander || true

                matrix-commander \
                  --login password \
                  --homeserver ${synapseHost} \
                  --user-login bot \
                  --password bot123 \
                  --room-default lounge \
                  --device matrix-commander \
                  --store bot-store \
                  --credentials bot-credentials.json || true

                matrix-commander --room-create lounge || true

                matrix-commander --room-invite '#lounge:synapse.test' --user '@bot:synapse.test' || true

                matrix-commander --message "Hello world!"
              '';
            };

            services.dendrite = {
              enable = true;
              httpPort = dendritePort;
              settings = {
                global.server_name = "dendrite.test";
                global.private_key = ./dendrite_key.pem;
              };
            };
          };
        };
    in
    {
      overlays.default = overlay;
    }
    //
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let
          pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [overlay];
            config.permittedInsecurePackages = ["olm-3.2.16"];
          };
          hps = hpsFor pkgs;
          name = "rhine-chat";
          libs = pkgs.buildEnv {
            name = "${name}-libs";
            paths = lib.mapCartesianProduct
              ({ hp, pname }: hp.${pname})
              { hp = attrValues hps; pname = pnames; };
            pathsToLink = [ "/lib" ];
          };
          docs = pkgs.buildEnv {
            name = "${name}-docs";
            paths = map (pname: pkgs.haskell.lib.documentationTarball hps.default.${pname}) pnames;
          };
          sdist = pkgs.buildEnv {
            name = "${name}-sdist";
            paths = map (pname: pkgs.haskell.lib.sdistTarball hps.default.${pname}) pnames;
          };
          docsAndSdist = pkgs.linkFarm "${name}-docsAndSdist" { inherit docs sdist; };
          all = pkgs.symlinkJoin {
            name = "${name}-all";
            paths = [ libs docsAndSdist ];
          };
        in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = pkgs;
          packages.${system} = {
            default = all;
            synapseVM = (pkgs.nixos synapseConfig).vm;
          };
          devShells.${system} = foreach hps (ghcName: hp: {
            ${ghcName} = hp.shellFor {
              packages = ps: map (pname: ps.${pname}) pnames;
              nativeBuildInputs = with hp; [
                pkgs'.haskellPackages.cabal-install
                fourmolu
                haskell-language-server
                pkgs.gomuks
                pkgs.nheko
                pkgs.cargo
              ];
            };
          });
        }
      );
}
