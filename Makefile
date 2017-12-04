all: Main
Main: Main.hs NNet.hs
	ghc Main.hs NNet.hs
clean:
	rm -f Main.hi NNet.hi Main.o NNet.o 
test: tests/test
	ghc NNet.hs Test.hs
