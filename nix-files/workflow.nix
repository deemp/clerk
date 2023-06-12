{ system, workflows, scripts, buildPrefix, ghcVersions }:
let
  job1 = "_1_nix_ci";
  job2 = "_2_build_with_ghc";
  job3 = "_3_push_to_cachix";
  inherit (workflows.configs.${system}) nixCI steps os oss on nixStore;
  inherit (workflows.functions.${system}) run expr mkAccessors genAttrsId installNix nixCI_ cacheNixDirs;
  names = mkAccessors { matrix = genAttrsId [ "os" "ghc" "store" ]; };
in
nixCI // {
  jobs = {
    "${job1}" = {
      name = "Write docs";
      runs-on = os.ubuntu-20;
      steps =
        [
          steps.checkout
          (installNix { store = nixStore.linux; })
          (cacheNixDirs { keySuffix = "docs"; store = nixStore.linux; restoreOnly = false; })
          steps.configGitAsGHActions
          steps.updateLocksAndCommit
          {
            name = "Write docs";
            run = run.nixRunAndCommit scripts.writeDocs.pname "Write docs";
          }
          steps.nixStoreCollectGarbage
        ];
    };
    "${job2}" = {
      name = "Build with GHC";
      strategy.matrix.ghc = ghcVersions;
      runs-on = os.ubuntu-20;
      steps = [
        steps.checkout
        (installNix { store = nixStore.linux; })
        (cacheNixDirs { keySuffix = "ghc"; store = nixStore.linux; restoreOnly = false; })
        (
          let ghc = expr names.matrix.ghc; in
          {
            name = "Build with ghc${ghc}";
            run = run.nixRun "${buildPrefix}${ghc}";
          }
        )
        steps.nixStoreCollectGarbage
      ];
    };
    "${job3}" = (nixCI_ []).jobs.nixCI;
  };
}
