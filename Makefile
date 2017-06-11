all:
	cabal install --bindir=./

.PHONY: clean
clean:
	rm -rf dist toyflow
