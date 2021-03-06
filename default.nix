{ mkDerivation, ghc, base, hashable, containers, pure-auth, pure-conjurer, pure-elm, pure-hooks, pure-json, pure-maybe, pure-render, pure-router, pure-shadows, pure-sorcerer, pure-sync, pure-tagsoup, pure-txt, pure-websocket, pure-websocket-cache, pure-xss-sanitize, pandoc, stdenv }:
mkDerivation {
  pname = "pure-convoker";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = 
    [ base hashable containers pure-auth pure-conjurer pure-elm pure-hooks pure-json pure-maybe pure-render pure-router pure-shadows pure-sorcerer pure-sync pure-tagsoup pure-txt pure-websocket pure-websocket-cache pure-xss-sanitize 
    ] ++ (if ghc.isGhcjs or false
      then [ ]
      else [ pandoc ]
    );
  license = stdenv.lib.licenses.bsd3;
  doHaddock = false;
}
