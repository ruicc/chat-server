make:
	# Here write a command you want to execute frequently!

	cabal build server -- --ghc-options="-O2 -ddump-prep -fforce-recomp --make -dsuppress-coercions" > core.server

clean:
	cabal clean

server:
	cabal build server > core.server
	cabal run server

client:
	cabal run client
