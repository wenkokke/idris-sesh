IDRIS2 ?= idris2
RUNTESTS := ./build/exec/runtests

.PHONY: test

test: $(RUNTESTS)
	$(RUNTESTS) $(IDRIS2)

$(RUNTESTS): Main.idr
	$(IDRIS2) --build tests.ipkg


.PHONY: clean

clean:
	@rm -rf build
	@find . -type f -name 'output' -exec rm -rf {} \;
	@find . -type f -name '*.ttc' -exec rm -f {} \;
	@find . -type f -name '*.ttm' -exec rm -f {} \;
	@find . -type f -name '*.ibc' -exec rm -f {} \;

