
J?=3

all: build test

build:
	jbuilder build @install -j $J

clean:
	jbuilder clean

doc:
	jbuilder build @doc

test:
	jbuilder runtest --no-buffer -j $J
	# ./tests/quick/all.sh # FIXME?

WATCH?=all
watch:
	while find src/ -print0 | xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		make $(WATCH); \
	done

ocp-indent:
	@which ocp-indent > /dev/null || { \
	  	echo 'ocp-indent not found; please run `opam install ocp-indent`'; \
		exit 1 ; \
	  }

reindent: ocp-indent
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 echo "reindenting: "
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 ocp-indent -i

.PHONY: all clean

