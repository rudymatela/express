# Makefile for express
#
# Copyright:   (c) 2015-2024 Rudy Matela
# License:     3-Clause BSD  (see the file LICENSE)
# Maintainer:  Rudy Matela <rudy@matela.com.br>
TESTS = \
  test/main \
  test/core \
  test/basic \
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
  bench/match-list \
  bench/match-noop \
  bench/match-triexpr \
  bench/pairs \
  bench/sort \
  bench/tiers \
  bench/tiers-complete \
  $(EGS)
GHCIMPORTDIRS = src:test
GHCFLAGS = -O2 -v0 $(shell grep -q "Arch Linux" /etc/lsb-release && echo -dynamic)
HUGSIMPORTDIRS = .:./src:./test:./etc/hugs-backports:/usr/lib/hugs/packages/*:../leancheck/src
HUGSFLAGS = -98 -h32M
RUNPARAMETERS =
LIB_DEPS = base template-haskell
INSTALL_DEPS = leancheck template-haskell

# to run a specific test suite under a specific GHC, do:
# $ make clean
# $ make GHC=ghc-9.6 GHCIMPORTDIRS=src:test:../leancheck/src test/utils.run
# this is useful for troubleshooting CI failures

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
	@rmdir bench/runtime/$$HOSTNAME/$<
	@printf "%-20s " $<
	@/usr/bin/time -f%e ./$< 2>&1 >/dev/null | tee bench/runtime/$$HOSTNAME/$<.runtime

clean: clean-hi-o clean-haddock
	rm -f $(TESTS) $(BENCHS) $(EGS) mk/toplibs

ghci: mk/All.ghci

hugs: src/Data/Express/Core.hugs

hugs-test: \
  test/typecheck.runhugs

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

diff-test: $(patsubst %,%.diff,$(BENCHS))

txt: $(patsubst %,%.txt,$(BENCHS))

%.diff: %
	./$< | diff -rud $<.txt -

%.txt: %
	./$< >$<.txt

test-via-everything: test test-via-cabal test-via-stack

test-on-ghc-9.10:
	make test GHC=ghc-9.10 GHCIMPORTDIRS=src:test:../leancheck/src

prepare:
	cabal update
	cabal install $(ALL_DEPS) --lib

hlint: ..hlint

test.hlint:  HLINT_EXTRA = --ignore "Redundant ==" \
                           --ignore "Use null" \
                           --ignore "Redundant $$" \
                           --ignore "Use isNothing"

%.hlint:
	hlint $(HLINT_EXTRA) \
	  --ignore "Use import/export shortcut" \
	  --ignore "Redundant bracket" \
	  --ignore "Use lambda-case" \
	  --ignore "Use typeRep" \
	  $*

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
