{ mkDerivation, base, pure, pure-json, pure-variance, stdenv }:
mkDerivation {
  pname = "pure-spacetime";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure pure-json pure-variance ];
  homepage = "github.com/grumply/pure-spacetime";
  description = "Styled views";
  license = stdenv.lib.licenses.bsd3;
}
