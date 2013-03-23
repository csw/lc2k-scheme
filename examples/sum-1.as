        lw   0 5 entry
        lw   0 7 stack
        jalr 5 6
        sw   0 1 SCMrv
SCMh    halt
        noop
Lcons   lw   0 5 heap
        lw   0 4 consS
        add  4 5 4
        sw   0 4 heap
        sw   4 1 0
        sw   4 2 1
        lw   0 5 ctag
        add  4 5 1
        jalr 6 5
        noop
Lcar    lw   0 5 pmask
        nand 1 5 1
        nand 1 1 1
        lw   1 1 0
        jalr 6 5
        noop
        noop
Lcdr    lw   0 5 pmask
        nand 1 5 1
        nand 1 1 1
        lw   1 1 1
        jalr 6 5
        noop    ; [not] '(code (v) (if (not (primcall %eq? #f v)) #f #t))
L0      lw      0       4       C0      ; #f   
        beq     4       1       I2     
        beq     0       0       I1     
I2      lw      0       1       C1      ; #t   
        jalr    6       4       ; return
I1      lw      0       1       C0      ; #f   
        jalr    6       4       ; return
        noop    ; [zero?] '(code (v) (if (primcall %zero? v) #t #f))
L1      beq     0       1       I8     
        lw      0       1       C0      ; #f   
        jalr    6       4       ; return
I8      lw      0       1       C1      ; #t   
        jalr    6       4       ; return
        noop    ; [negative?] '(code (v) (if (primcall %zero? (primcall %band v %sign-bit)) #f #t))
L2      lw      0       5       C2      ; %sign-bit
        nand    1       5       2              
        nand    2       2       2       (temp 1) = (local v) & (constant 2147483648 %sign-bit)
        beq     0       2       I13    
        lw      0       1       C1      ; #t   
        jalr    6       4       ; return
I13     lw      0       1       C0      ; #f   
        jalr    6       4       ; return
        noop    ; [positive?] '(code (v) (if (or (primcall %zero? v) (not (primcall %eq? #f (call not (call zero? (primcall %band v %sign-bit)))))) #f #t))
L3      lw      0       4       C3     
        add     7       4       7       ; SP -= 5
        beq     0       1       I20    
        lw      0       5       C2      ; %sign-bit
        nand    1       5       2              
        nand    2       2       2       (temp 1) = (local v) & (constant 2147483648 %sign-bit)
        sw      7       6       5       ; store saved value return-addr
        add     0       2       1       ; marshal arg 1
        lw      0       5       A1      ; load address of zero?
        jalr    5       6               ; call (zero? (temp 1))
        lw      0       5       A0      ; load address of not
        jalr    5       6               ; call (not (temp 2))
        add     0       1       2       ; move result to (temp 3)
        lw      7       6       5       ; load restored value return-addr
        lw      0       4       C0      ; #f   
        beq     4       2       I21    
        beq     0       0       I20    
I21     lw      0       1       C1      ; #t   
        lw      0       4       C4     
        add     7       4       7       ; SP += 5
        jalr    6       4       ; return
I20     lw      0       1       C0      ; #f   
        lw      0       4       C4     
        add     7       4       7       ; SP += 5
        jalr    6       4       ; return
        noop    ; [even?] '(code (v) (call zero? (primcall %band v 1)))
L4      lw      0       4       C3     
        add     7       4       7       ; SP -= 5
        lw      0       5       C5      ; 1    
        nand    1       5       2              
        nand    2       2       2       (temp 1) = (local v) & (constant 1 1)
        add     0       2       1       ; marshal arg 1
        lw      0       4       C4     
        add     7       4       7       ; SP += 5
        lw      0       4       A1      ; load target address
        jalr    4       5       ; tail-call (zero? (temp 1))
        noop    ; [odd?] '(code (v) (if (primcall %zero? (primcall %band v 1)) #f #t))
L5      lw      0       5       C5      ; 1    
        nand    1       5       2              
        nand    2       2       2       (temp 1) = (local v) & (constant 1 1)
        beq     0       2       I37    
        lw      0       1       C1      ; #t   
        jalr    6       4       ; return
I37     lw      0       1       C0      ; #f   
        jalr    6       4       ; return
        noop    ; [+] '(code (x y) (primcall %add x y))
L6      add     1       2       3       (temp 0) = (local x) + (local y)
        add     0       3       1       ; place return value
        jalr    6       4       ; return
        noop    ; [boolean?] '(code (v) (if (primcall %eq? (primcall %band %type-tag-mask v) %bool-tag) #t #f))
L7      lw      0       4       C6      ; %type-tag-mask
        nand    4       1       2              
        nand    2       2       2       (temp 1) = (constant 4261412864 %type-tag-mask) & (local v)
        lw      0       5       C0      ; %bool-tag
        beq     2       5       I46    
        lw      0       1       C0      ; #f   
        jalr    6       4       ; return
I46     lw      0       1       C1      ; #t   
        jalr    6       4       ; return
        noop    ; [char?] '(code (v) (if (primcall %eq? (primcall %band %type-tag-mask v) %char-tag) #t #f))
L8      lw      0       4       C6      ; %type-tag-mask
        nand    4       1       2              
        nand    2       2       2       (temp 1) = (constant 4261412864 %type-tag-mask) & (local v)
        lw      0       5       C7      ; %char-tag
        beq     2       5       I53    
        lw      0       1       C0      ; #f   
        jalr    6       4       ; return
I53     lw      0       1       C1      ; #t   
        jalr    6       4       ; return
        noop    ; [eq?] '(code (obj1 obj2) (if (primcall %eq? obj1 obj2) #t #f))
L9      beq     1       2       I60    
        lw      0       1       C0      ; #f   
        jalr    6       4       ; return
I60     lw      0       1       C1      ; #t   
        jalr    6       4       ; return
        noop    ; [empty?] '(code (v) (if (primcall %eq? v ()) #t #f))
L10     lw      0       5       C8      ; ()   
        beq     1       5       I65    
        lw      0       1       C0      ; #f   
        jalr    6       4       ; return
I65     lw      0       1       C1      ; #t   
        jalr    6       4       ; return
        noop    ; [null?] '(code (v) (if (primcall %eq? v ()) #t #f))
L11     lw      0       5       C8      ; ()   
        beq     1       5       I70    
        lw      0       1       C0      ; #f   
        jalr    6       4       ; return
I70     lw      0       1       C1      ; #t   
        jalr    6       4       ; return
        noop    ; [pair?] '(code (v) (if (primcall %eq? (primcall %band %type-tag-mask v) %cons-tag) #t #f))
L12     lw      0       4       C6      ; %type-tag-mask
        nand    4       1       2              
        nand    2       2       2       (temp 1) = (constant 4261412864 %type-tag-mask) & (local v)
        lw      0       5       C9      ; %cons-tag
        beq     2       5       I75    
        lw      0       1       C0      ; #f   
        jalr    6       4       ; return
I75     lw      0       1       C1      ; #t   
        jalr    6       4       ; return
        noop    ; [integer?] '(code (v) (if (primcall %eq? (primcall %band %tagged-mask v) %tagged-tag) #f #t))
L13     lw      0       4       C10     ; %tagged-mask
        nand    4       1       2              
        nand    2       2       2       (temp 1) = (constant 3221225472 %tagged-mask) & (local v)
        lw      0       5       C7      ; %tagged-tag
        beq     2       5       I82    
        lw      0       1       C1      ; #t   
        jalr    6       4       ; return
I82     lw      0       1       C0      ; #f   
        jalr    6       4       ; return
        noop    ; [number?] '(code (v) (if (primcall %eq? (primcall %band %tagged-mask v) %tagged-tag) #f #t))
L14     lw      0       4       C10     ; %tagged-mask
        nand    4       1       2              
        nand    2       2       2       (temp 1) = (constant 3221225472 %tagged-mask) & (local v)
        lw      0       5       C7      ; %tagged-tag
        beq     2       5       I89    
        lw      0       1       C1      ; #t   
        jalr    6       4       ; return
I89     lw      0       1       C0      ; #f   
        jalr    6       4       ; return
        noop    ; [procedure?] '(code (v) (if (primcall %eq? (primcall %band %type-tag-mask v) %proc-tag) #t #f))
L15     lw      0       4       C6      ; %type-tag-mask
        nand    4       1       2              
        nand    2       2       2       (temp 1) = (constant 4261412864 %type-tag-mask) & (local v)
        lw      0       5       C11     ; %proc-tag
        beq     2       5       I96    
        lw      0       1       C0      ; #f   
        jalr    6       4       ; return
I96     lw      0       1       C1      ; #t   
        jalr    6       4       ; return
        noop    ; [sum] '(code (l acc) (if (primcall %eq? l ()) acc (call sum (call cdr l) (primcall %add (call car l) acc))))
L16     lw      0       4       C3     
        add     7       4       7       ; SP -= 5
I102    sw      7       6       5       ; store spilled value return-addr
        lw      0       5       C8      ; ()   
        beq     1       5       I103   
        sw      7       1       4       ; store saved value (local l)
        sw      7       2       3       ; store saved value (local acc)
        lw      7       1       4       ; load arg value (local l)
        lw      0       5       Acdr    ; load address of cdr
        jalr    5       6               ; call (cdr (local l))
        add     0       1       3       ; move result to (temp 1)
        sw      7       3       2       ; store saved value (temp 1)
        lw      7       1       4       ; load arg value (local l)
        lw      0       5       Acar    ; load address of car
        jalr    5       6               ; call (car (local l))
        add     0       1       6       ; move result to (temp 2)
        lw      7       2       3       ; load restored value (local acc)
        add     6       2       1       (temp 3) = (temp 2) + (local acc)
        sw      7       1       4       ; store arg value #f
        lw      7       1       2       ; load arg value (temp 1)
        lw      7       2       4       ; load arg value (temp 3)
        lw      7       6       5       ; load spilled value return-addr
        beq     0       0       I102    ; tail-call (<self> (temp 1) (temp 3))
I103    lw      7       5       5       ; load spilled value return-addr
        add     0       2       1       ; place return value
        lw      0       4       C4     
        add     7       4       7       ; SP += 5
        jalr    5       4       ; return
        noop    ; [%toplevel] '(code () (call sum (call cons 1 (call cons 2 (call cons 3 (call cons 4 (call cons 5 empty))))) 0))
L17     lw      0       4       C3     
        add     7       4       7       ; SP -= 5
        sw      7       6       5       ; store saved value return-addr
        lw      0       1       C4     
        lw      0       2       C8     
        lw      0       5       Acons   ; load address of cons
        jalr    5       6               ; call (cons (constant 5 5) (constant 1140850688 empty))
        sw      7       1       4       ; store arg value #f
        lw      0       1       C12    
        lw      7       2       4       ; load arg value (temp 1)
        lw      0       5       Acons   ; load address of cons
        jalr    5       6               ; call (cons (constant 4 4) (temp 1))
        add     0       1       2       ; move result to (temp 2)
        lw      0       1       C13    
        lw      0       5       Acons   ; load address of cons
        jalr    5       6               ; call (cons (constant 3 3) (temp 2))
        sw      7       1       4       ; store arg value #f
        lw      0       1       C14    
        lw      7       2       4       ; load arg value (temp 3)
        lw      0       5       Acons   ; load address of cons
        jalr    5       6               ; call (cons (constant 2 2) (temp 3))
        add     0       1       2       ; move result to (temp 4)
        lw      0       1       C5     
        lw      0       5       Acons   ; load address of cons
        jalr    5       6               ; call (cons (constant 1 1) (temp 4))
        lw      0       2       C15    
        lw      7       6       5       ; load restored value return-addr
        lw      0       4       C4     
        add     7       4       7       ; SP += 5
        lw      0       4       A16     ; load target address
        jalr    4       5       ; tail-call (sum (temp 5) (constant 0 0))
stack   .fill 65535
heapS   .fill 8192
heap    .fill 8192
SCMrv   .fill 559038737
consS   .fill 2
entry   .fill L17
ctag    .fill 1610612736
pmask   .fill 65535
C10     .fill   3221225472
C2      .fill   2147483648
C8      .fill   1140850688
C12     .fill   4
C5      .fill   1
C9      .fill   1610612736
C7      .fill   1073741824
C6      .fill   4261412864
C15     .fill   0
C14     .fill   2
C4      .fill   5
C3      .fill   -5
C0      .fill   1107296256
C11     .fill   1744830464
C1      .fill   1107296257
C13     .fill   3
Acons    .fill Lcons
Pcons    .fill 1761607686
Acar    .fill Lcar
Pcar    .fill 1761607696
Acdr    .fill Lcdr
Pcdr    .fill 1761607703
A0    .fill L0
P0    .fill 1761607709
A1    .fill L1
P1    .fill 1761607717
A2    .fill L2
P2    .fill 1761607723
A3    .fill L3
P3    .fill 1761607732
A4    .fill L4
P4    .fill 1761607758
A5    .fill L5
P5    .fill 1761607769
A6    .fill L6
P6    .fill 1761607778
A7    .fill L7
P7    .fill 1761607782
A8    .fill L8
P8    .fill 1761607792
A9    .fill L9
P9    .fill 1761607802
A10    .fill L10
P10    .fill 1761607808
A11    .fill L11
P11    .fill 1761607815
A12    .fill L12
P12    .fill 1761607822
A13    .fill L13
P13    .fill 1761607832
A14    .fill L14
P14    .fill 1761607842
A15    .fill L15
P15    .fill 1761607852
A16    .fill L16
P16    .fill 1761607862
A17    .fill L17
P17    .fill 1761607891
