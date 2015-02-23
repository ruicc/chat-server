make:
	# Here write a command you want to execute frequently!

#	cabal build server -- --ghc-options="-O2 -ddump-prep -fforce-recomp --make -dsuppress-coercions" > core.server

conf:
	cabal configure --enable-library-profiling --enable-executable-profiling --enable-tests --enable-benchmarks

clean:
	cabal clean

server:
	cabal build server > core.server
	cabal run server

client:
	cabal run client

prof:
	cabal run mem-prof > /dev/null 2>&1
	hp2ps -e8in -c mem-prof.hp
	cp mem-prof.ps /vagrant
