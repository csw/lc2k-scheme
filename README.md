# LC2K Scheme

This is a [Scheme][] compiler for the LC2K processor, an imaginary
32-bit RISC CPU used for the introductory computer organization course
at the University of Michigan, EECS 370. The LC2K is a fairly minimal
design, where the only ALU operations are `add` and `nand`. After
writing assembly programs for it by hand and with ad hoc
code-generation scripts, I was curious to see what it took to run a
real programming language on it.

The compiler is implemented in [Racket][], a Scheme dialect, and uses
a Ruby script to run the LC2K assembler and simulator. The assembler
and simulator are *not* included; they are part of class projects and
not, as far as I know, freely available.

# Status

This compiler implements a subset of Scheme, including:

- functions
- first-class functions
- integers (-2^31 to 2^30-1)
- booleans
- pairs (aka conses, aka linked lists)
- heap allocation
- characters
- proper tail calls
- `if`, `and`, `or`, `not`
- type predicates
- various numeric functions: `+`, `-`, `even?`, `zero?`, `negative?`

It does *not* yet implement:

- functions with more than 3 arguments (coming up next)
- closures and lambda forms
- quoted literals
- `let`, `letrec`
- `set!`
- vectors
- symbols
- strings
- error handling
- type checks
- multiplication
- garbage collection
- etc...

# Usage

## Simple compilation

With the test file [examples/fib.scm](examples/fib.scm):

```scheme
(define (fib-aux n a b)
  (if (zero? n)
      a
      (fib-aux (- n 1) b (+ a b))))

(define (fib n)
  (fib-aux n 0 1))

(fib 21)
```

We can compile this from the comment line with:

    $ racket -t compiler.rkt examples/fib.scm > fib.as

This will write out LC2K [assembly](examples/fib.as) suitable for
assembling and running on a simulator. When the machine halts, the
result will be encoded as a Scheme value in register 1, and stored to
the location labeled `SCMrv` in the assembly code.

## Running code

However, it's easier to use the included driver code to compile,
assemble, and run Scheme code, and parse and display the result.

This uses the `runner` Ruby program behind the scenes, so make sure
you have Ruby 1.9 or later available. (If you don't already have Ruby,
it's very easy to install with [RVM][]).

Set the environment variables `LC2K`, `ASM`, and `SIM` to the
directory where your LC2K assembler and simulator live, and the names
of the assembler and simulator programs. For instance, to use the
'solution' programs:

    $ export LC2K=/path/to/370/code
    $ export ASM=asol
    $ export SIM=ssol

Now, use `driver.rkt`:

    $ racket -t driver.rkt examples/fib.scm
    10946
    $ racket -t driver.rkt examples/foldl.scm
    15

Admittedly, the output isn't very exciting, but you can see from the
source code that it's correct.

# Examples

To see lists and first-class functions in action, see
[examples/foldl.scm](examples/foldl.scm):

```scheme
(define (foldl proc init lst)
  (if (empty? lst)
      init
      (foldl proc
             (proc init (car lst))
             (cdr lst))))

(foldl +
       0
       (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 empty))))))
```

And the corresponding [assembly code](examples/foldl.as).

(For non-Lispers, the standard lists are linked lists created with
`cons`; each cons cell has a head, accessed with `car`, and a tail,
accessed with `cdr`, which should generally be another cons cell or
the special value `empty`. This builds the list `(1 2 3 4 5)`, then
sums its elements.)

# Requirements

- Racket (tested with version 5.3.3)
- Ruby (tested with 1.9.3)
- LC2K assembler and simulator

# LC2K in a nutshell

The LC2K is a 32-bit word-addressed RISC machine with eight registers
(0 through 7, and register 0 always holds 0), a 16-bit address space,
and eight instructions.

Instruction set:
 - `add`: adds integers
 - `nand`: bitwise NAND
 - `lw`: load word, base+offset addressing
 - `sw`: store word, base+offset addressing
 - `beq`: branch if equal
 - `jalr`: jump and link register
 - `noop`: what it says on the tin
 - `halt`: likewise

## Lisp implementation considerations for the LC2K

Having 16-bit address space with a 32-bit word size means there are
plenty of extra bits for type tags on pointers, so I use a
tagged-pointer representation.

Lisp implementations usually use a few low bits as tag bits, and use
right shifts to turn minimally-tagged integers into machine integers,
but the lack of a shifter on the LC2K makes this impractical. Instead,
the two high bits are `01` to indicate a tagged word (anything other
than an integer). This lets us use native machine integers as Scheme
integers; the tag bits can be masked off of pointers as necessary, in
only a few instructions.

See [types.rkt](types.rkt) for more details on the data
representations.

# Resources

Having never written a compiler before, I found several resources
invaluable for this project. 

Abdulaziz Ghuloum's
[An Incremental Approach to Compiler Construction][ghuloum-paper]
paper provided a very useful roadmap, although I took a less
stack-centric approach. Matt Might's
[Compiling Scheme to C with closure conversion][might-post] was a
useful guide to the front end, although using C as a target certainly
simplifies matters.

[An Introduction to Scheme and its Implementation][intro-scheme-impl]
had excellent implementation insights, as did R. Kent Dybvig's
[ICFP talk][dybvig-talk] on the development of Chez Scheme.

Pereira's [A Survey on Register Allocation][pereira-survey] led me to
choose a linear scan register allocator. The original
[paper][linear-scan] by Poletto and Sarkar was very useful. However,
another [paper][sagonas] by Sagonas and Stenman on its use with the
Erlang HiPE compiler had key insights for using it in practice.

[Scheme]: http://en.wikipedia.org/wiki/Scheme_(programming_language)
[Racket]: http://racket-lang.org/
[RVM]: https://rvm.io/
[ghuloum-paper]: http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf
[might-post]: http://matt.might.net/articles/compiling-scheme-to-c/
[intro-scheme-impl]: http://icem-www.folkwang-hochschule.de/~finnendahl/cm_kurse/doc/schintro/schintro_toc.html
[dybvig-talk]: http://icfp06.cs.uchicago.edu/dybvig-talk.pdf
[pereira-survey]: http://compilers.cs.ucla.edu/fernando/publications/drafts/survey.pdf
[linear-scan]: http://www.seas.gwu.edu/~hchoi/teaching/cs160d/linearscan.pdf
[sagonas]: http://onlinelibrary.wiley.com/doi/10.1002/spe.533/abstract

