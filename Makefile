
TARGET = server
CORES = 4

make:
	git submodule init
	git submodule update
	stack build

#core:
#	stack build --ghc-options="-O2 -ddump-simpl -fforce-recomp --make -dsuppress-coercions" > core.$(TARGET)

clean:
	stack clean
	rm -f core.$(TARGET)
	rm -f $(TARGET).*

repl:
	stack ghci
#	cabal repl $(TARGET)

prof:
	stack exec $(TARGET) -- +RTS -N$(CORES) -p -s -h -i1

run:
	stack exec $(TARGET) -- +RTS -N$(CORES)

ps:
	hp2ps -e8in -c $(TARGET).hp
	cp $(TARGET).ps /vagrant


# Helper commands
server:
	make run TARGET=server

client:
	make run TARGET=client

