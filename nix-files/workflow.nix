{ system, workflows, scripts, buildPrefix, ghcVersions}:
let
  job1 = "_1_nix_ci";
  job2 = "_2_build_with_ghc";
  job3 = "_3_push_to_cachix";
  inherit (workflows.configs.${system}) nixCI steps os oss on;
  inherit (workflows.functions.${system}) run expr mkAccessors genAttrsId;
  names = mkAccessors { matrix = genAttrsId [ "os" "ghc" ]; };
in
nixCI // {
  jobs = {
    "${job1}" = {
      name = "Update flake locks and README.md";
      runs-on = os.ubuntu-20;
      steps =
        [
          steps.checkout
          steps.installNix
          steps.configGitAsGHActions
          steps.updateLocksAndCommit
          {
            name = "Write docs";
            run = run.nixRunAndCommit scripts.writeDocs.pname "Write docs";
          }
        ];
    };
    "${job2}" = {
      name = "Build with GHC";
      strategy.matrix.ghc = ghcVersions;
      # needs = job1;
      runs-on = os.ubuntu-20;
      steps = [
        steps.checkout
        steps.installNix
        {
          name = "Pull repo";
          run = "git pull --rebase --autostash";
        }
        (
          let ghc = expr names.matrix.ghc; in
          {
            name = "Build with ghc${ghc}";
            run = ''nix run .#${buildPrefix}${ghc}'';
          }
        )
      ];
    };
    "${job3}" = {
      name = "Push to cachix";
      # needs = job1;
      strategy.matrix.os = oss;
      runs-on = expr names.matrix.os;
      steps =
        [
          steps.checkout
          steps.installNix
          steps.logInToCachix
          steps.pushFlakesToCachix
        ];
    };
  };
}
