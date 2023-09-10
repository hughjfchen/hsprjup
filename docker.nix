{ nativePkgs ? (import ./default.nix {}).pkgs,
crossBuildProject ? import ./cross-build.nix {} }:
nativePkgs.lib.mapAttrs (_: prj:
with prj.hsprjup;
let
  executable = hsprjup.hsprjup.components.exes.hsprjup;
  binOnly = prj.pkgs.runCommand "hsprjup-bin" { } ''
    mkdir -p $out/bin
    cp ${executable}/bin/hsprjup $out/bin
    ${nativePkgs.nukeReferences}/bin/nuke-refs $out/bin/hsprjup
    printf "%s %s\n" "$out/bin/hsprjup-exe" '"''$@" +RTS -N''${K8S_CPU_LIMIT-3} -M''${K8S_MEMORY_LIMIT-1024M} -A''${GHC_GEN_0_SIZE-16M} -RTS' > $out/bin/hsprjup_entry.sh
    chmod +x $out/bin/hsprjup_entry.sh
  '';
in { 
  hsprjup-image = prj.pkgs.dockerTools.buildImage {
  name = "hsprjup";
  tag = executable.version;
  contents = [ binOnly prj.pkgs.cacert prj.pkgs.iana-etc ];
  config.Entrypoint = "hsprjup_entry.sh";
  config.Cmd = "";
  };
}) crossBuildProject
