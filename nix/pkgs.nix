# our packages overlay
final: prev: with final; {

  inherit (cardanoNodeProject.args) compiler-nix-name;

  # The is used by nix/regenerate.sh to pre-compute package list to avoid double evaluation.
  genProjectPackages = lib.genAttrs
    (lib.attrNames (haskell-nix.haskellLib.selectProjectPackages
      cardanoNodeProject.hsPkgs))
    (name: lib.attrNames cardanoNodeProject.pkg-set.options.packages.value.${name}.components.exes);

  cabal = haskell-nix.tool compiler-nix-name "cabal" {
    version = "latest";
    inherit (cardanoNodeProject) index-state;
  };

  hlint = haskell-nix.tool compiler-nix-name "hlint" {
    version = "3.2.7";
    inherit (cardanoNodeProject) index-state;
  };

  ghcid = haskell-nix.tool compiler-nix-name "ghcid" {
    version = "0.8.7";
    inherit (cardanoNodeProject) index-state;
  };

  haskell-language-server = haskell-nix.tool compiler-nix-name "haskell-language-server" {
    version = "latest";
    inherit (cardanoNodeProject) index-state;
  };

  haskellBuildUtils = prev.haskellBuildUtils.override {
    inherit compiler-nix-name;
    inherit (cardanoNodeProject) index-state;
  };

  cardanolib-py = callPackage ./cardanolib-py { };

  scripts = lib.recursiveUpdate (import ./scripts.nix { inherit pkgs; })
    (import ./scripts-submit-api.nix { inherit pkgs; });

  clusterTests = import ./workbench/tests { inherit pkgs; };

  plutus-scripts = callPackage ./plutus-scripts.nix { plutus-builder = plutus-example; };

  dockerImage =
    let
      defaultConfig = {
        stateDir = "/data";
        dbPrefix = "db";
        socketPath = "/ipc/node.socket";
      };
    in
    callPackage ./docker {
      exe = "cardano-node";
      scripts = import ./scripts.nix {
        inherit pkgs;
        customConfigs = [ defaultConfig customConfig ];
      };
      script = "node";
    };

  submitApiDockerImage =
    let
      defaultConfig = {
        socketPath = "/node-ipc/node.socket";
        listenAddress = "0.0.0.0";
      };
    in
    callPackage ./docker/submit-api.nix {
      exe = "cardano-submit-api";
      scripts = import ./scripts-submit-api.nix {
        inherit pkgs;
        customConfigs = [ defaultConfig customConfig ];
      };
      script = "submit-api";
    };

  # A generic, parameteric version of the workbench development environment.
  workbench = pkgs.callPackage ./workbench {};

  all-profiles-json = (workbench.all-profiles
    { inherit (workbench-instance { backendName = "supervisor";
                                    profileName = "default-bage";
                                  }) backend; }).JSON;

  # A parametrisable workbench, that can be used with nix-shell or lorri.
  # See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  workbench-instance =
    let backendRegistry =
          {
            supervisor = {
              backend-workbench = ./workbench/backend/supervisor.nix;
              workbench-runner  = ./workbench/backend/supervisor-run.nix;
            };
            nomad = {
              backend-workbench = ./workbench/backend/nomad.nix;
              workbench-runner  = ./workbench/backend/nomad-run.nix;
            };
          };
    in
    { backendName
    , profileName           ? customConfig.localCluster.profileName
    , batchName             ? customConfig.localCluster.batchName
    , useCabalRun           ? false
    , workbenchDevMode      ? false
    , profiled              ? false
    , workbench             ? pkgs.workbench
    , backendWorkbench      ? pkgs.callPackage (backendRegistry."${backendName}".backend-workbench) { inherit useCabalRun workbench; }
    , cardano-node-rev      ? null
    }:
    pkgs.callPackage (backendRegistry."${backendName}".workbench-runner)
      {
        inherit batchName profileName backendWorkbench cardano-node-rev;
      };

  # Disable failing python uvloop tests
  python38 = prev.python38.override {
    packageOverrides = pythonFinal: pythonPrev: {
      uvloop = pythonPrev.uvloop.overrideAttrs (attrs: {
        disabledTestPaths = [ "tests/test_tcp.py" "tests/test_sourcecode.py" "tests/test_dns.py" ];
      });
    };
  };
}
