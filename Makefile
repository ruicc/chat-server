
TARGET = server
CORES = 4

make:
	# Here write a command you want to execute frequently!

cabal.sandbox.config:
	cabal sandbox init
	cabal install --only-dependencies

tmp/structured-concurrent: cabal.sandbox.config
	mkdir -p tmp
	git clone https://github.com/ruicc/structured-concurrent.git tmp/structured-concurrent
	cabal sandbox add-source tmp/structured-concurrent

core:
	cabal build $(TARGET) -- --ghc-options="-O2 -ddump-simpl -fforce-recomp --make -dsuppress-coercions" > core.$(TARGET)

conf:
	cabal configure --enable-library-profiling --enable-executable-profiling --enable-tests --enable-benchmarks

clean:
	cabal clean
	rm -f core.$(TARGET)
	rm -f $(TARGET).*

repl:
	cabal repl $(TARGET)

prof:
	cabal run $(TARGET) -- +RTS -N$(CORES) -p -s -h -i1
	make tops

run: tmp/structured-concurrent
	cabal run $(TARGET) -- +RTS -N$(CORES)

ps:
	hp2ps -e8in -c $(TARGET).hp
	cp $(TARGET).ps /vagrant


# Helper commands
server:
	make run TARGET=server

client:
	make run TARGET=client

