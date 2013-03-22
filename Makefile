EXAMPLE_ASM = $(patsubst %.scm,%.as,$(wildcard examples/*.scm))

examples: $(EXAMPLE_ASM)

%.as: %.scm
	racket -t compiler.rkt $< > $@
