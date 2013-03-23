EXAMPLE_ASM = $(patsubst %.scm,%.as,$(wildcard examples/*.scm))

examples: $(EXAMPLE_ASM)

%.as: %.scm *.rkt
	racket -t compiler.rkt $< > $@
