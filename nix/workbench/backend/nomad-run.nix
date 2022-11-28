let
  backendName        = "nomad";
  batchNameDefault   = "plain";
  profileNameDefault = "default-bage";
in
{ pkgs
, cardanoNodePackages
, backendWorkbench
##
, profileName           ? profileNameDefault
, batchName             ? batchNameDefault
##
, workbenchDevMode      ? false
, cardano-node-rev      ? "0000000000000000000000000000000000000000"
}:
let
  inherit (backendWorkbench) workbench backend cacheDir stateDir basePort;

  with-backend-profile =
    { envArgsOverride ? {} }: ## TODO: envArgsOverride is not used!
    workbench.with-profile
      { inherit backend profileName; };

  inherit (with-backend-profile {}) profileNix profile topology genesis;
in
  let

    inherit (profile.value) era composition monetary;

    path = pkgs.lib.makeBinPath path';
    path' =
      [ cardanoNodePackages.bech32 pkgs.jq pkgs.gnused pkgs.coreutils pkgs.bash pkgs.moreutils
      ]
      ## In dev mode, call the script directly:
      ++ pkgs.lib.optionals (!workbenchDevMode)
      [ workbench.workbench ];

    interactive-start = pkgs.writeScriptBin "start-cluster" ''
      set -euo pipefail

      export PATH=$PATH:${path}
      unset WB_MODE_CABAL=
      wb start \
        --batch-name   ${batchName} \
        --profile-name ${profileName} \
        --profile      ${profile} \
        --cache-dir    ${cacheDir} \
        --base-port    ${toString basePort} \
        ''${WB_MODE_CABAL:+--cabal} \
        "$@"
    '';

    interactive-stop = pkgs.writeScriptBin "stop-cluster" ''
      set -euo pipefail

      wb finish "$@"
    '';

    interactive-restart = pkgs.writeScriptBin "restart-cluster" ''
      set -euo pipefail

      wb run restart "$@" && \
        echo "workbench:  alternate command for this action:  wb run restart" >&2
    '';

    nodeBuildProduct =
      name:
      "report ${name}-log $out ${name}/stdout";

    profile-run =
      { trace ? false }:
      let
        inherit
          (with-backend-profile
            { envArgsOverride = { cacheDir = "./cache"; stateDir = "./"; }; })
          profileNix profile topology genesis;

        run = pkgs.runCommand "workbench-run-${backendName}-${profileName}"
          { requiredSystemFeatures = [ "benchmark" ];
            nativeBuildInputs = with cardanoNodePackages; with pkgs; [
              bash
              bech32
              coreutils
              gnused
              jq
              moreutils
              nixWrapped
              pstree
# TODO:       nomad
              workbench.workbench
              zstd
            ];
          }
            ''
            mkdir -p    $out/{cache,nix-support}
            cd          $out
            export HOME=$out

            export WB_BACKEND=${backendName}
            export CARDANO_NODE_SOCKET_PATH=$(wb backend get-node-socket-path ${stateDir} node-0)

            cmd=(
              wb
              ${pkgs.lib.optionalString trace "--trace"}
              start
              --profile-name        ${profileName}
              --profile             ${profile}
              --topology            ${topology}
              --genesis-cache-entry ${genesis}
              --batch-name          smoke-test
              --base-port           ${toString basePort}
              --node-source         ${cardanoNodePackages.cardano-node.src.origSrc}
              --node-rev            ${cardano-node-rev}
              --cache-dir           ./cache
            )
            echo "''${cmd[*]}" > $out/wb-start.sh

            time "''${cmd[@]}" 2>&1 |
              tee $out/wb-start.log

            ## Convert structure from $out/run/RUN-ID/* to $out/*:
            rm -rf cache
            rm -f run/{current,-current}
            find $out -type s | xargs rm -f
            run=$(cd run; ls)
            (cd run; tar c $run --zstd) > archive.tar.zst
            mv       run/$run/*  .
            rmdir    run/$run    run

            cat > $out/nix-support/hydra-build-products <<EOF
            report workbench-log   $out wb-start.log
            report meta            $out meta.json
            ${pkgs.lib.concatStringsSep "\n"
              (map nodeBuildProduct (__attrNames profileNix.node-specs.value))}
            report archive-tar-zst $out archive.tar.zst
            EOF

            echo "workbench-test:  completed run $run"
            '';
  in
    run // {
      analysis = workbench.run-analysis { inherit pkgs workbench profileNix run; };
    };
in
{
  inherit stateDir;
  inherit profileName;
  inherit workbench backendWorkbench;
  inherit (backendWorkbench) backend;
  inherit profileNix profile topology genesis;
  inherit interactive-start interactive-stop interactive-restart;
  inherit profile-run;
}
