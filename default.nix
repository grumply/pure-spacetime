{ mkDerivation, base, pure, pure-json, pure-variance, stdenv }:
mkDerivation {
  pname = "pure-space-time";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure pure-json pure-variance ];
  homepage = "github.com/grumply/pure-space-time";
  description = "Styled views";
  license = stdenv.lib.licenses.bsd3;
}