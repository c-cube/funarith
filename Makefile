
J?=3

all: build test

build:
	@dune build @install -j $J

clean:
	@dune clean

doc:
	@dune build @doc

test:
	@dune runtest --force --no-buffer -j $J

WATCH?=@all
watch:
	@dune build $(WATCH) -w

ocp-indent:
	@which ocp-indent > /dev/null || { \
	  	echo 'ocp-indent not found; please run `opam install ocp-indent`'; \
		exit 1 ; \
	  }

reindent: ocp-indent
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 echo "reindenting: "
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 ocp-indent -i

.PHONY: all clean

