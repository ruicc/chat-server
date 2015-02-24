CORE =
REPL =
PROF =

make:
	# Here write a command you want to execute frequently!

core:
	cabal build $(CORE) -- --ghc-options="-O2 -ddump-prep -fforce-recomp --make -dsuppress-coercions" > core.$(CORE)

conf:
	cabal configure --enable-library-profiling --enable-executable-profiling --enable-tests --enable-benchmarks

clean:
	cabal clean
	rm -f mem-prof.*

server:
	cabal build server > core.server
	cabal run server

client:
	cabal run client

repl:
	cabal repl $(REPL)

prof:
	cabal run $(PROF) > /dev/null 2>&1
	hp2ps -e8in -c $(PROF).hp
	cp $(PROF).ps /vagrant
