{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    systems.url = "github:nix-systems/default";
    devenv.url = "github:cachix/devenv";
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
                # ./devenv.nix
                {
                  # https://devenv.sh/basics/
                  env.GREET = "devenv";
                  # env.PATH = "node_modules/.bin:$PATH";
                  # https://devenv.sh/packages/
                  packages = [ 
                    pkgs.git 
                    pkgs.yarn 
                    # pkgs.elm
                  ];

                  enterShell = ''
                    echo ""
                    echo "  Welcome"
                    echo ""

                    # python -m venv .venv
                    # source .venv/bin/activate
                    # pip install -r requirements.txt
                    # echo .venv >> .gitignore

                    # yarn
                    # echo node_modules >> .gitignore

                  '';

                  # https://devenv.sh/languages/
                  # languages.rust.enable = true;
                  # languages.python.enable = true;
                  languages.elm.enable = true;
                  languages.javascript.enable = true;


                  # https://devenv.sh/scripts/
                  # scripts.hello.exec = "echo hello from $GREET";

                  # https://devenv.sh/pre-commit-hooks/
                  # pre-commit.hooks.shellcheck.enable = true;

                  # https://devenv.sh/processes/
                  # processes.ping.exec = "ping example.com";
                }
              ];
            };
          });
    };
}
