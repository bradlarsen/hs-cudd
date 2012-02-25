PATH := $(HOME)/Library/Haskell/bin:$(HOME)/bin:/opt/local/bin:$(PATH)

.PHONY: all install test tags clean

all: tags install test

tags:
	find . -iname '*.hs' -o -iname '*.hsc' -o -iname '*.lhs' | xargs hasktags --ctags -x -f .tags
	ctags -R --append=yes -f .tags

install:
	cabal install

test:
	cabal test

clean:
	rm -rf dist
	rm -f tags
