.PHONY: all clean test

DUNE       := dune
EXECUTABLE := driver.exe

all:
	$(DUNE) build src/$(EXECUTABLE)

exec:
	rlwrap $(DUNE) exec src/$(EXECUTABLE) -- -lex

clean:
	$(DUNE) clean

test: all
	echo "(1 + 2 * 10) * 2" | ./_build/default/src/$(EXECUTABLE)
