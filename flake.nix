{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      perSystem = { pkgs, ... }: with pkgs; {
        devShells.default = mkShell {
          packages = [
            janet
            jpm
          ];

          buildInputs = [
            libX11
            libXcursor
            libXi
            libXinerama
            libXrandr
          ];

          shellHook = ''
            export JANET_PATH="$PWD/.jpm"
            export JANET_TREE="$JANET_PATH/tree"
            export JANET_LIBPATH="${janet}/lib"
            export JANET_HEADERPATH="${janet}/include/janet"
            export JANET_BUILDPATH="$JANET_PATH/build"
            export PATH="$JANET_TREE/bin:$PATH"
            mkdir -p "$JANET_TREE"
            mkdir -p "$JANET_BUILDPATH"

            export LD_LIBRARY_PATH="''${LD_LIBRARY_PATH}''${LD_LIBRARY_PATH:+:}${libglvnd}/lib"
          '';
        };
      };
    };
}
