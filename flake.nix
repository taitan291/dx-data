{
  description = "R command-line environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { nixpkgs, ... }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
      pkgsFor = system: import nixpkgs { inherit system; };
    in
    {
      devShells = forAllSystems (system:
        let pkgs = pkgsFor system;
        in {
          default = pkgs.mkShell {
            packages = [ pkgs.R ];
            FONTCONFIG_FILE = pkgs.makeFontsConf {
              fontDirectories = [ pkgs.dejavu_fonts pkgs.noto-fonts-cjk-sans ];
            };
          };
        });

      apps = forAllSystems (system:
        let pkgs = pkgsFor system;
        in {
          default = {
            type = "app";
            program = "${pkgs.R}/bin/R";
          };
          Rscript = {
            type = "app";
            program = "${pkgs.R}/bin/Rscript";
          };
        });
    };
}
