.PHONY: all install test tags clean

all: tags install test

tags:
	find . -iname '*.hs' -o -iname '*.hsc' | xargs hasktags --ctags
	find . -iname '*.[ch]' -o -iname '*.cpp' | xargs ctags --append --sort=yes

install:
	cabal install

test:
	cabal test

clean:
	rm -rf dist
	rm -f tags
