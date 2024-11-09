DEP_FILES = $(shell find . -name "*.dep")
BYTECODE_FILES = $(shell find . -name "*.zo")
ENTRY_POINT = main.rkt

.PHONY: all clean run test

all:
	raco make $(ENTRY_POINT)
clean:
	rm -f $(DEP_FILES) $(BYTECODE_FILES)

run: all
	racket $(ENTRY_POINT)

test: all
	raco test .