{ mkDerivation, ghc, base, hashable, containers, pure-conjurer, pure-elm, pure-json, pure-render, stdenv }:
mkDerivation {
  pname = "pure-convoker";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = 
    [ base hashable containers pure-conjurer pure-elm pure-json pure-render ];
  license = stdenv.lib.licenses.bsd3;
}
