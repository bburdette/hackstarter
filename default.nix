{ cabal, aeson, conduit, dataDefault, fastLogger, hjsmin, hspec
, httpConduit, monadControl, monadLogger, persistent
, persistentSqlite, persistentTemplate, purescript, resourcet, shakespeare
, text, transformers, waiExtra, waiLogger, warp, yaml, yesod
, yesodAuth, yesodCore, yesodForm, yesodStatic, yesodTest, yesodBin
}:

cabal.mkDerivation (self: {
  pname = "hackstarter";
  version = "0.0.0";
  src=./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson conduit dataDefault fastLogger hjsmin httpConduit
    monadControl monadLogger persistent persistentSqlite
    persistentTemplate shakespeare text waiExtra waiLogger warp yaml
    yesod yesodAuth yesodCore yesodForm yesodStatic
  ];
  buildTools = [ yesodBin purescript ];
  testDepends = [
    hspec monadLogger persistent persistentSqlite resourcet
    transformers yesod yesodCore yesodTest
  ];
  meta = {
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
