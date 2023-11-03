{
  inputs = {
    # nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    systems.url = "github:nix-systems/default";
    devenv.url = "github:cachix/devenv";
  };

  nixConfig = {
    extra-trusted-public-keys = "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://devenv.cachix.org";
  };

  outputs = { self, nixpkgs, devenv, systems, ... } @ inputs:
    let
      forEachSystem = nixpkgs.lib.genAttrs (import systems);
    in
    {
      devShells = forEachSystem
        (system:
          let
            pkgs = nixpkgs.legacyPackages.${system};
          in
          {
            default = devenv.lib.mkShell {
              inherit inputs pkgs;
              modules = [
                {
                  # https://devenv.sh/reference/options/
                  packages = with pkgs; [ 
                    yarn
                    bun

                  ];

                  enterShell = ''
                    echo ""
                    echo "  - Start working on goban"
                    echo "    Run 'yarn dev' to run the app"
                    echo ""
                    export PATH="$PWD/bin:$PWD/node_modules/.bin:$PATH"
                    yarn install
                  '';

                  languages.javascript.enable = true;
                  difftastic.enable = true;
                  hosts = {
                    "example.com" = "127.0.0.1";
                  };
                }
              ];
            };
          });
    };
}
