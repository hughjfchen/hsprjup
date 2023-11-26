{ defaultPlatformProject ? import ./default.nix { }, exactDeps ? true
, withShellHook ? true, homeDir ? builtins.getEnv "HOME" }:
let
  mkCabalConfig = { configFiles }:
    let originalConfig = builtins.readFile "${configFiles}/cabal.config";
    in defaultPlatformProject.pkgs.writeText "cabal.config" ''
      ${originalConfig}
      package-db: ${homeDir}/.cabal/store/ghc-${defaultPlatformProject.hsprjup.ghc.identifier.version}/package.db
    '';
  mkShell = shellHookPara:
    defaultPlatformProject.hsprjup.shellFor {
      # Include only the *local* packages of your project.
      packages = ps:
        with ps;
        [
          hsprjup
          #pkgb
        ];

      # Builds a Hoogle documentation index of all dependencies,
      # and provides a "hoogle" command to search the index.
      withHoogle = true;

      # You might want some extra tools in the shell (optional).

      # Some common tools can be added with the `tools` argument
      #tools = { cabal = "latest"; hlint = "latest"; };
      tools = {
        cabal =
          defaultPlatformProject.pkgs.haskell-nix.cabal-install.ghc928.version;
        hasktags = "latest";
        hlint = "latest";
        fourmolu = "latest";
        #haskell-language-server = "latest";
      };
      # See overlays/tools.nix for more details

      # Some you may need to get some other way.
      buildInputs = with defaultPlatformProject.pkgs;
      #[ ghcid lorri niv ];
        [ ghcid ];

      # Prevents cabal from choosing alternate plans, so that
      # *all* dependencies are provided by Nix.
      inherit exactDeps;

      # add shellHook to workaround cabal build issue
      shellHook = shellHookPara;
    };
  baseShell = mkShell "";
  fixedShellHook = baseShell.shellHook
    + defaultPlatformProject.pkgs.lib.optionalString withShellHook ''
      export CABAL_CONFIG=${
        mkCabalConfig { configFiles = baseShell.configFiles; }
      }
    '';
  fixedShell = mkShell fixedShellHook;

in fixedShell
