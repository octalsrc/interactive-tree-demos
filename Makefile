all:
	cabal sandbox init
	git clone "https://github.com/GHCJS/GHCJS-JQuery.git"
	git clone "https://github.com/GHCJS/GHCJS-Canvas.git"
	cabal install --ghcjs GHCJS-JQuery/ GHCJS-Canvas/
	cabal install --ghcjs
	mkdir www
	cp ./rotate.html www/rotate.html
	cp .cabal-sandbox/bin/rotate-demo.jsexe/all.js www/rotate.js
	cp ./heap.html www/heap.html
	cp .cabal-sandbox/bin/heap-demo.jsexe/all.js www/heap.js
	cp ./twoheaps.html www/twoheaps.html
	cp .cabal-sandbox/bin/twoheaps-demo.jsexe/all.js www/twoheaps.js
	cp ./animate.html www/animate.html
	cp .cabal-sandbox/bin/animate-test.jsexe/all.js www/animate.js
