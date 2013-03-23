EXAMPLE_ASM = $(patsubst %.scm,%.as,$(wildcard examples/*.scm))

.PHONY: test

test:
	racket -t test.rkt

examples: $(EXAMPLE_ASM)

%.as: %.scm *.rkt
	racket -t compiler.rkt $< > $@
