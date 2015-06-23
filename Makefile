MFLAGS =
MAKEFLAGS = $(MFLAGS)
SANDBOX = .cabal-sandbox
CABAL_FLAGS =
DEPS = .cabal-sandbox/.foo

.PHONY: build test tags clean clobber

default: test

${SUBMODULE_DEPS}:
	git submodule init
	git submodule update

${SANDBOX}: ${SUBMODULE_DEPS}
	cabal sandbox init

${DEPS}: ${SANDBOX} $(wildcard *.cabal)
	cabal install -j --only-dependencies --enable-tests
	cabal configure --enable-tests ${CABAL_FLAGS}
	touch $@

build: ${DEPS}
	cabal build --ghc-option="-Werror"

test: ${DEPS}
	cabal test quickcheck --log=/dev/stdout

hlint: ${DEPS}
	cabal test hlint --log=/dev/stdout

tags:
	hasktags -e src test main

clean:
	cabal clean

clobber:
	cabal clean
	cabal sandbox delete
