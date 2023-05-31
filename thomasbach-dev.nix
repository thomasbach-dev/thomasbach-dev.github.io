{ mkDerivation, base, hakyll, lib, text, witch }:
mkDerivation {
  pname = "thomasbach-dev";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hakyll text witch ];
  homepage = "https://thomasbach.dev/";
  description = "My website";
  license = lib.licenses.bsd3;
  mainProgram = "thomasbach-dev";
}
