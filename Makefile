all: build

build:
	@dune build @install

clean:
	@rm -rf `find . -name 'bisect*.out'` _coverage
	@dune clean

coverage: clean
	@BISECT_ENABLE=yes dune runtest

format:
	@dune build @fmt

install: build
	@dune install

test:
	@dune runtest --force

.PHONY: all build clean coverage test
