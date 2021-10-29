{ mkDerivation, ghc, base, hashable, containers, pure-auth, pure-conjurer, pure-elm, pure-json, pure-maybe, pure-render, pure-sync, pure-websocket, pure-websocket-cache, stdenv }:
mkDerivation {
  pname = "pure-convoker";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = 
    [ base hashable containers pure-auth pure-conjurer pure-elm pure-json pure-maybe pure-render pure-sync pure-websocket pure-websocket-cache ];
  license = stdenv.lib.licenses.bsd3;
}
