PATH := $(HOME)/Library/Haskell/bin:$(HOME)/bin:/opt/local/bin:$(PATH)

.PHONY: all install build configure tags test clean

all: build

tags:
	find src test cudd-2.5.0 -iname '*.hs' -o -iname '*.hsc' -o -iname '*.lhs' \
		| xargs hasktags --ctags -x -f .tags
	ctags -R --append=yes -f .tags

configure:
	cabal configure --enable-tests

build: configure
	cabal build

install: build
	cabal install

test:
	cabal test

clean:
	rm -rf dist
	rm -f .tags
