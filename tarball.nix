{ nativePkgs ? (import ./default.nix {}).pkgs,
crossBuildProject ? import ./cross-build.nix {} }:
nativePkgs.lib.mapAttrs (_: prj:
with prj.hsprjup;
let
  executable = hsprjup.hsprjup.components.exes.hsprjup;
  binOnly = prj.pkgs.runCommand "hsprjup-bin" { } ''
    mkdir -p $out/bin
    cp -R ${executable}/bin/* $out/bin/
    ${nativePkgs.nukeReferences}/bin/nuke-refs $out/bin/hsprjup
  '';

  tarball = nativePkgs.stdenv.mkDerivation {
    name = "hsprjup-tarball";
    buildInputs = with nativePkgs; [ zip ];

    phases = [ "installPhase" ];

    installPhase = ''
      mkdir -p $out/
      zip -r -9 $out/hsprjup-tarball.zip ${binOnly}
    '';
  };
in {
 hsprjup-tarball = tarball;
}
) crossBuildProject
