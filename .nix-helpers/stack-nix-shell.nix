# This is the shell file specified in the stack.yaml file.
# This runs stack commands in an environment created with nix.

with (import ./nixpkgs.nix { });

haskell.lib.buildStackProject {
  name = "termonad";
  buildInputs = [
    cairo
    git
    gnome3.vte
    gobjectIntrospection
    gtk3
    zlib
  ];
  ghc = haskell.compiler.ghc844;
  extraArgs = [
    "--stack-yaml stack-lts-12.yaml"
  ];
}
