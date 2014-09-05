let pkgs = import <nixpkgs> {};
    haskellPackages = pkgs.haskellPackages.override {
      extension = self: super: {
        hackstarter = self.callPackage ./. {};
      };
    };
 in pkgs.lib.overrideDerivation haskellPackages.hackstarter (attrs: {
   buildInputs = [ haskellPackages.cabalInstall ] ++ attrs.buildInputs;
 })
