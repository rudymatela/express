# Makefile for express
#
# Copyright:   (c) 2015-2021 Rudy Matela
# License:     3-Clause BSD  (see the file LICENSE)
# Maintainer:  Rudy Matela <rudy@matela.com.br>
TESTS = \
  test/main \
  test/core \
  test/map \
  test/instances \
  test/fixtures \
  test/express \
  test/express-derive \
  test/name \
  test/name-derive \
  test/utils \
  test/canon \
  test/match \
  test/hole \
  test/fold \
  test/show \
  test/ord \
  test/triexpr \
  test/listable
EGS =
BENCHS = \
  eg/u-extrapolate \
  eg/u-speculate \
  eg/u-conjure \
  bench/compare \
  bench/exprs \
  bench/match \
  bench/pairs \
  bench/sort \
  bench/tiers \
  bench/triexpr \
  $(EGS)
GHCIMPORTDIRS = src:test
GHCFLAGS = -O2 $(shell grep -q "Arch Linux" /etc/lsb-release && echo -dynamic)
HUGSIMPORTDIRS = .:./src:./test:./etc/hugs-backports:/usr/lib/hugs/packages/*
HUGSFLAGS = -98 -h32M
RUNPARAMETERS =
LIB_DEPS = base template-haskell
INSTALL_DEPS = leancheck

all: mk/toplibs

all-all: mk/All.o

test: $(patsubst %,%.run,$(TESTS)) test-sdist diff-test

slow-test: RUNPARAMETERS=50400
slow-test: test

%.run: %
	./$< $(RUNPARAMETERS)

.PHONY: bench
bench: $(patsubst %,%.bench,$(BENCHS))
	@mkdir -p bench/runtime/$$HOSTNAME
	./bench/versions | tee bench/runtime/$$HOSTNAME/versions

.PHONY: %.bench
%.bench: %
	@mkdir -p bench/runtime/$$HOSTNAME/$<
	@printf "%-18s " $<
	@/usr/bin/time -f%e ./$< 2>&1 >/dev/null | tee bench/runtime/$$HOSTNAME/$<.runtime

clean: clean-hi-o clean-haddock
	rm -f $(TESTS) $(BENCHS) $(EGS) mk/toplibs

ghci: mk/All.ghci

install:
	@echo "use \`cabal install' instead"

test-sdist:
	./test/sdist

test-via-cabal:
	cabal configure --enable-tests --enable-benchmarks --ghc-options="$(GHCFLAGS) -O0"
	cabal build
	cabal test main

test-via-stack:
	stack test express:test:main --ghc-options="$(GHCFLAGS) -O0" --system-ghc --no-install-ghc --no-terminal

diff-test: $(patsubst %,%.diff-test,$(BENCHS))

update-diff-test: $(patsubst %,%.update-diff-test,$(BENCHS))

%.diff-test: %
	./$< | diff -rud test/model/$<.out -

%.update-diff-test: %
	./$< >           test/model/$<.out

test-via-everything: test test-via-cabal test-via-stack

hugs-test:
	echo 'TBA'

legacy-test: # needs ghc-8.8 .. ghc-7.8 installed as such
	make clean  &&  make test -j GHC=ghc-8.8
	make clean  &&  make test -j GHC=ghc-8.6
	make clean  &&  make test -j GHC=ghc-8.4
	make clean  &&  make test -j GHC=ghc-8.2
	make clean  &&  make test -j GHC=ghc-8.0
	make clean  &&  make test -j GHC=ghc-7.10
	make clean  &&  make test -j GHC=ghc-7.8
	make clean  &&  make test -j

legacy-test-via-cabal: # needs similarly named cabal wrappers
	cabal clean  &&  cabal-ghc-8.8  configure  &&  cabal-ghc-8.8  test
	cabal clean  &&  cabal-ghc-8.6  configure  &&  cabal-ghc-8.6  test
	cabal clean  &&  cabal-ghc-8.4  configure  &&  cabal-ghc-8.4  test
	cabal clean  &&  cabal-ghc-8.2  configure  &&  cabal-ghc-8.2  test
	cabal clean  &&  cabal-ghc-8.0  configure  &&  cabal-ghc-8.0  test
	cabal clean  &&  cabal-ghc-7.10 configure  &&  cabal-ghc-7.10 test
	cabal clean  &&  cabal-ghc-7.8  configure  &&  cabal-ghc-7.8  test
	cabal clean  &&  cabal test

prepare:
	cabal update
	cabal install $(ALL_DEPS) --lib

prepare-legacy-test:
	cabal update
	cabal-ghc-8.8  install $(ALL_DEPS) --lib
	cabal-ghc-8.6  install $(ALL_DEPS) --lib
	cabal-ghc-8.4  install $(ALL_DEPS) --lib
	cabal-ghc-8.2  install $(ALL_DEPS) --lib
	cabal-ghc-8.0  install $(ALL_DEPS) --lib
	cabal-ghc-7.10 v1-install $(ALL_DEPS)
	cabal-ghc-7.8  v1-install $(ALL_DEPS)
	# (v2-) library installation is supported on GHC 8.0+ only)

hlint:
	hlint \
	  --ignore "Use import/export shortcut" \
	  --ignore "Redundant bracket" \
	  .

markdown: README.html

%.html: %.md
	pandoc $< -o $@

# NOTE: (very hacky!) the following target allows parallel compilation (-jN) of
# eg and test programs so long as they don't share dependencies _not_ stored
# in src/ and test/.  Runnable binaries should depend on mk/toplibs instead of
# actual Haskell source files
mk/toplibs: mk/Toplibs.o
	touch mk/toplibs

include mk/haskell.mk
