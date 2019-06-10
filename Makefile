.PHONY: all clean exec test test1

DUNE       := dune
EXECUTABLE := driver.exe

all: driver

lib:
	$(DUNE) build src/lib/tiger.lib

driver:
	$(DUNE) build src/bin/driver.exe

exec:
	rlwrap $(DUNE) exec src/bin/$(EXECUTABLE)

test:
	$(DUNE) runtest

clean:
	$(DUNE) clean

test1: all
	echo "(1 + 2 * 10) * 2" | ./_build/default/src/bin/$(EXECUTABLE)
