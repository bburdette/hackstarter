{ cabal, aeson, conduit, dataDefault, esqueleto, fastLogger, hjsmin
, hspec, httpConduit, monadControl, monadLogger, persistent
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
  buildTools = [ purescript yesodBin ];
  buildDepends = [
    aeson conduit dataDefault esqueleto fastLogger hjsmin httpConduit
    monadControl monadLogger persistent persistentSqlite
    persistentTemplate shakespeare text transformers waiExtra waiLogger
    warp yaml yesod yesodAuth yesodCore yesodForm yesodStatic
  ];
  testDepends = [
    hspec monadLogger persistent persistentSqlite resourcet
    transformers yesod yesodCore yesodTest
  ];
  meta = {
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
