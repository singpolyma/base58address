GHCFLAGS=-Wall -fno-warn-name-shadowing -XHaskell98 -O2
HLINTFLAGS=-XHaskell98 -XCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension' --utf8
VERSION=0.2

.PHONY: all shell clean doc install

all: report.html doc dist/build/libHSbase58address-$(VERSION).a dist/base58address-$(VERSION).tar.gz

install: dist/build/libHSbase58address-$(VERSION).a
	cabal install

shell:
	ghci $(GHCFLAGS)

report.html: Data/Base58Address.hs Data/Base58Address/BaseConvert.hs Data/Base58Address/Alphabet.hs
	-hlint $(HLINTFLAGS) --report $^

doc: dist/doc/html/base58address/index.html README

README: base58address.cabal
	tail -n+$$(( `grep -n ^description: $^ | head -n1 | cut -d: -f1` + 1 )) $^ > .$@
	head -n+$$(( `grep -n ^$$ .$@ | head -n1 | cut -d: -f1` - 1 )) .$@ > $@
	-printf ',s/        //g\n,s/^.$$//g\n,s/\\\\\\//\\//g\nw\nq\n' | ed $@
	$(RM) .$@

dist/doc/html/base58address/index.html: dist/setup-config Data/Base58Address.hs Data/Base58Address/BaseConvert.hs Data/Base58Address/Alphabet.hs
	cabal haddock --hyperlink-source

dist/setup-config: base58address.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist

dist/build/libHSbase58address-$(VERSION).a: dist/setup-config Data/Base58Address.hs Data/Base58Address/BaseConvert.hs Data/Base58Address/Alphabet.hs
	cabal build --ghc-options="$(GHCFLAGS)"

dist/base58address-$(VERSION).tar.gz: README dist/setup-config Data/Base58Address.hs Data/Base58Address/BaseConvert.hs Data/Base58Address/Alphabet.hs
	cabal check
	cabal sdist
