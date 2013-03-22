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
L0      lw      0       4       C2     
        add     7       4       7       ; SP -= 5
        lw      0       2       C0      ; (temp 1) = #f
        beq     2       1       I2     
        beq     0       0       I1     
I2      lw      0       2       C1      ; (temp 0) = #t
        add     0       2       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    6       4       ; return
I1      lw      0       2       C0      ; (temp 0) = #f
        add     0       2       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    6       4       ; return
        noop    ; [zero?] '(code (v) (if (primcall %zero? v) #t #f))
L1      lw      0       4       C2     
        add     7       4       7       ; SP -= 5
        beq     0       1       I8     
        lw      0       1       C0      ; (temp 0) = #f
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    6       4       ; return
I8      lw      0       1       C1      ; (temp 0) = #t
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    6       4       ; return
        noop    ; [negative?] '(code (v) (if (primcall %zero? (primcall %band v %sign-bit)) #f #t))
L2      lw      0       4       C2     
        add     7       4       7       ; SP -= 5
        lw      0       2       C4      ; (temp 1) = %sign-bit
        nand    1       2       3      
        nand    3       3       3      
        beq     0       3       I13    
        lw      0       2       C1      ; (temp 0) = #t
        add     0       2       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    6       4       ; return
I13     lw      0       2       C0      ; (temp 0) = #f
        add     0       2       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    6       4       ; return
        noop    ; [positive?] '(code (v) (if (or (primcall %zero? v) (not (primcall %eq? #f (call not (call zero? (primcall %band v %sign-bit)))))) #f #t))
L3      lw      0       4       C2     
        add     7       4       7       ; SP -= 5
        beq     0       1       I20    
        lw      0       2       C4      ; (temp 1) = %sign-bit
        nand    1       2       3      
        nand    3       3       3      
        sw      7       6       5       ; store saved value return-addr
        add     0       3       1       ; marshal arg 1
        lw      0       5       A1      ; load address of zero?
        jalr    5       6       ; call zero?
        add     0       1       2       ; move result
        add     0       2       1       ; marshal arg 1
        lw      0       5       A0      ; load address of not
        jalr    5       6       ; call not
        add     0       1       3       ; move result
        lw      0       2       C0      ; (temp 5) = #f
        lw      7       6       5       ; load restored value return-addr
        beq     2       3       I21    
        beq     0       0       I20    
I21     lw      0       2       C1      ; (temp 0) = #t
        add     0       2       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    6       4       ; return
I20     lw      0       2       C0      ; (temp 0) = #f
        add     0       2       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    6       4       ; return
        noop    ; [even?] '(code (v) (call zero? (primcall %band v 1)))
L4      lw      0       4       C2     
        add     7       4       7       ; SP -= 5
        lw      0       2       C5      ; (temp 1) = 1
        nand    1       2       3      
        nand    3       3       3      
        add     0       3       1       ; marshal arg 1
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        lw      0       4       A1      ; load target address
        jalr    4       5       ; tail-call zero?
        noop    ; [odd?] '(code (v) (call not (call zero? (primcall %band v 1))))
L5      lw      0       4       C2     
        add     7       4       7       ; SP -= 5
        lw      0       2       C5      ; (temp 1) = 1
        nand    1       2       3      
        nand    3       3       3      
        sw      7       6       5       ; store saved value return-addr
        add     0       3       1       ; marshal arg 1
        lw      0       5       A1      ; load address of zero?
        jalr    5       6       ; call zero?
        add     0       1       2       ; move result
        add     0       2       1       ; marshal arg 1
        lw      7       6       5       ; load restored value return-addr
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        lw      0       4       A0      ; load target address
        jalr    4       5       ; tail-call not
        noop    ; [+] '(code (x y) (primcall %add x y))
L6      add     1       2       3      
        add     0       3       1       ; place return value
        jalr    6       4       ; return
        noop    ; [boolean?] '(code (v) (if (primcall %eq? (primcall %band %type-tag-mask v) %bool-tag) #t #f))
L7      lw      0       4       C2     
        add     7       4       7       ; SP -= 5
        sw      7       6       5       ; store spilled value return-addr
        lw      0       2       C0      ; (temp 1) = %bool-tag
        lw      0       3       C6      ; (temp 2) = %type-tag-mask
        nand    3       1       6      
        nand    6       6       6      
        beq     6       2       I43    
        lw      0       3       C0      ; (temp 0) = #f
        lw      7       5       5       ; load spilled value return-addr
        add     0       3       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    5       4       ; return
I43     lw      0       3       C1      ; (temp 0) = #t
        lw      7       5       5       ; load spilled value return-addr
        add     0       3       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    5       4       ; return
        noop    ; [char?] '(code (v) (if (primcall %eq? (primcall %band %type-tag-mask v) %char-tag) #t #f))
L8      lw      0       4       C2     
        add     7       4       7       ; SP -= 5
        sw      7       6       5       ; store spilled value return-addr
        lw      0       2       C7      ; (temp 1) = %char-tag
        lw      0       3       C6      ; (temp 2) = %type-tag-mask
        nand    3       1       6      
        nand    6       6       6      
        beq     6       2       I50    
        lw      0       3       C0      ; (temp 0) = #f
        lw      7       5       5       ; load spilled value return-addr
        add     0       3       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    5       4       ; return
I50     lw      0       3       C1      ; (temp 0) = #t
        lw      7       5       5       ; load spilled value return-addr
        add     0       3       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    5       4       ; return
        noop    ; [eq?] '(code (obj1 obj2) (if (primcall %eq? obj1 obj2) #t #f))
L9      lw      0       4       C2     
        add     7       4       7       ; SP -= 5
        beq     1       2       I57    
        lw      0       2       C0      ; (temp 0) = #f
        add     0       2       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    6       4       ; return
I57     lw      0       2       C1      ; (temp 0) = #t
        add     0       2       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    6       4       ; return
        noop    ; [empty?] '(code (v) (if (primcall %eq? v ()) #t #f))
L10     lw      0       4       C2     
        add     7       4       7       ; SP -= 5
        lw      0       2       C8      ; (temp 1) = ()
        beq     1       2       I62    
        lw      0       2       C0      ; (temp 0) = #f
        add     0       2       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    6       4       ; return
I62     lw      0       2       C1      ; (temp 0) = #t
        add     0       2       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    6       4       ; return
        noop    ; [null?] '(code (v) (if (primcall %eq? v ()) #t #f))
L11     lw      0       4       C2     
        add     7       4       7       ; SP -= 5
        lw      0       2       C8      ; (temp 1) = ()
        beq     1       2       I67    
        lw      0       2       C0      ; (temp 0) = #f
        add     0       2       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    6       4       ; return
I67     lw      0       2       C1      ; (temp 0) = #t
        add     0       2       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    6       4       ; return
        noop    ; [pair?] '(code (v) (if (primcall %eq? (primcall %band %type-tag-mask v) %cons-tag) #t #f))
L12     lw      0       4       C2     
        add     7       4       7       ; SP -= 5
        sw      7       6       5       ; store spilled value return-addr
        lw      0       2       C9      ; (temp 1) = %cons-tag
        lw      0       3       C6      ; (temp 2) = %type-tag-mask
        nand    3       1       6      
        nand    6       6       6      
        beq     6       2       I72    
        lw      0       3       C0      ; (temp 0) = #f
        lw      7       5       5       ; load spilled value return-addr
        add     0       3       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    5       4       ; return
I72     lw      0       3       C1      ; (temp 0) = #t
        lw      7       5       5       ; load spilled value return-addr
        add     0       3       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    5       4       ; return
        noop    ; [integer?] '(code (v) (if (primcall %eq? (primcall %band %tagged-mask v) %tagged-tag) #f #t))
L13     lw      0       4       C2     
        add     7       4       7       ; SP -= 5
        sw      7       6       5       ; store spilled value return-addr
        lw      0       2       C7      ; (temp 1) = %tagged-tag
        lw      0       3       C10     ; (temp 2) = %tagged-mask
        nand    3       1       6      
        nand    6       6       6      
        beq     6       2       I79    
        lw      0       3       C1      ; (temp 0) = #t
        lw      7       5       5       ; load spilled value return-addr
        add     0       3       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    5       4       ; return
I79     lw      0       3       C0      ; (temp 0) = #f
        lw      7       5       5       ; load spilled value return-addr
        add     0       3       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    5       4       ; return
        noop    ; [number?] '(code (v) (if (primcall %eq? (primcall %band %tagged-mask v) %tagged-tag) #f #t))
L14     lw      0       4       C2     
        add     7       4       7       ; SP -= 5
        sw      7       6       5       ; store spilled value return-addr
        lw      0       2       C7      ; (temp 1) = %tagged-tag
        lw      0       3       C10     ; (temp 2) = %tagged-mask
        nand    3       1       6      
        nand    6       6       6      
        beq     6       2       I86    
        lw      0       3       C1      ; (temp 0) = #t
        lw      7       5       5       ; load spilled value return-addr
        add     0       3       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    5       4       ; return
I86     lw      0       3       C0      ; (temp 0) = #f
        lw      7       5       5       ; load spilled value return-addr
        add     0       3       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    5       4       ; return
        noop    ; [procedure?] '(code (v) (if (primcall %eq? (primcall %band %type-tag-mask v) %proc-tag) #t #f))
L15     lw      0       4       C2     
        add     7       4       7       ; SP -= 5
        sw      7       6       5       ; store spilled value return-addr
        lw      0       2       C11     ; (temp 1) = %proc-tag
        lw      0       3       C6      ; (temp 2) = %type-tag-mask
        nand    3       1       6      
        nand    6       6       6      
        beq     6       2       I93    
        lw      0       3       C0      ; (temp 0) = #f
        lw      7       5       5       ; load spilled value return-addr
        add     0       3       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    5       4       ; return
I93     lw      0       3       C1      ; (temp 0) = #t
        lw      7       5       5       ; load spilled value return-addr
        add     0       3       1       ; place return value
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        jalr    5       4       ; return
        noop    ; [foldl] '(code (proc init lst) (if (primcall %eq? lst ()) init (call foldl proc (call proc init (call car lst)) (call cdr lst))))
L16     lw      0       4       C12    
        add     7       4       7       ; SP -= 6
I99     sw      7       6       6       ; store spilled value return-addr
        sw      7       2       1       ; store spilled value (local init)
        lw      0       6       C8      ; (temp 1) = ()
        beq     3       6       I100   
        sw      7       1       5       ; store saved value (local proc)
        sw      7       3       3       ; store saved value (local lst)
        lw      7       1       3       ; load arg value (local lst)
        lw      0       5       Acar    ; load address of car
        jalr    5       6       ; call car
        add     0       1       6       ; move result
        lw      7       1       5       ; load restored value (local proc)
        lw      0       4       C13     ; load pointer mask
        nand    4       1       5      
        nand    5       5       5      
        sw      7       1       5       ; store saved value (local proc)
        lw      7       1       1       ; load arg value (local init)
        add     0       6       2       ; marshal arg 2
        jalr    5       6       ; call (local proc)
        add     0       1       2       ; move result
        sw      7       2       4       ; store saved value (temp 3)
        lw      7       1       3       ; load arg value (local lst)
        lw      0       5       Acdr    ; load address of cdr
        jalr    5       6       ; call cdr
        add     0       1       6       ; move result
        lw      7       1       5       ; load arg value (local proc)
        lw      7       2       4       ; load arg value (temp 3)
        add     0       6       3       ; marshal arg 3
        lw      7       6       6       ; load spilled value return-addr
        beq     0       0       I99     ; self-tail-call
I100    lw      7       1       1       ; load spilled value (local init)
        lw      7       5       6       ; load spilled value return-addr
        lw      0       4       C14    
        add     7       4       7       ; SP += 6
        jalr    5       4       ; return
        noop    ; [%toplevel] '(code () (call foldl + 0 (call cons 1 (call cons 2 (call cons 3 (call cons 4 (call cons 5 empty)))))))
L17     lw      0       4       C2     
        add     7       4       7       ; SP -= 5
        lw      0       1       C3      ; (temp 1) = 5
        lw      0       2       C8      ; (temp 2) = empty
        sw      7       6       5       ; store saved value return-addr
        lw      0       5       Acons   ; load address of cons
        jalr    5       6       ; call cons
        add     0       1       3       ; move result
        lw      0       2       C15     ; (temp 4) = 4
        add     0       2       1       ; marshal arg 1
        add     0       3       2       ; marshal arg 2
        lw      0       5       Acons   ; load address of cons
        jalr    5       6       ; call cons
        lw      0       2       C16     ; (temp 6) = 3
        sw      7       1       4       ; store arg value #f
        add     0       2       1       ; marshal arg 1
        lw      7       2       4       ; load arg value (temp 5)
        lw      0       5       Acons   ; load address of cons
        jalr    5       6       ; call cons
        add     0       1       3       ; move result
        lw      0       2       C17     ; (temp 8) = 2
        add     0       2       1       ; marshal arg 1
        add     0       3       2       ; marshal arg 2
        lw      0       5       Acons   ; load address of cons
        jalr    5       6       ; call cons
        lw      0       2       C5      ; (temp 10) = 1
        sw      7       1       4       ; store arg value #f
        add     0       2       1       ; marshal arg 1
        lw      7       2       4       ; load arg value (temp 9)
        lw      0       5       Acons   ; load address of cons
        jalr    5       6       ; call cons
        add     0       1       3       ; move result
        lw      0       2       P6      ; (temp 12) = &+
        lw      0       1       C18     ; (temp 13) = 0
        sw      7       1       4       ; store arg value #f
        add     0       2       1       ; marshal arg 1
        lw      7       2       4       ; load arg value (temp 13)
        lw      7       6       5       ; load restored value return-addr
        lw      0       4       C3     
        add     7       4       7       ; SP += 5
        lw      0       4       A16     ; load target address
        jalr    4       5       ; tail-call foldl
stack   .fill 65535
heapS   .fill 8192
heap    .fill 8192
SCMrv   .fill 559038737
consS   .fill 2
entry   .fill L17
ctag    .fill 1610612736
pmask   .fill 65535
C14     .fill   6
C10     .fill   3221225472
C4      .fill   2147483648
C8      .fill   1140850688
C15     .fill   4
C5      .fill   1
C9      .fill   1610612736
C7      .fill   1073741824
C12     .fill   -6
C6      .fill   4261412864
C18     .fill   0
C17     .fill   2
C3      .fill   5
C2      .fill   -5
C0      .fill   1107296256
C11     .fill   1744830464
C1      .fill   1107296257
C13     .fill   65535
C16     .fill   3
Acons    .fill Lcons
Pcons    .fill 1761607686
Acar    .fill Lcar
Pcar    .fill 1761607696
Acdr    .fill Lcdr
Pcdr    .fill 1761607703
A0    .fill L0
P0    .fill 1761607709
A1    .fill L1
P1    .fill 1761607725
A2    .fill L2
P2    .fill 1761607737
A3    .fill L3
P3    .fill 1761607754
A4    .fill L4
P4    .fill 1761607784
A5    .fill L5
P5    .fill 1761607795
A6    .fill L6
P6    .fill 1761607812
A7    .fill L7
P7    .fill 1761607816
A8    .fill L8
P8    .fill 1761607837
A9    .fill L9
P9    .fill 1761607858
A10    .fill L10
P10    .fill 1761607872
A11    .fill L11
P11    .fill 1761607887
A12    .fill L12
P12    .fill 1761607902
A13    .fill L13
P13    .fill 1761607923
A14    .fill L14
P14    .fill 1761607944
A15    .fill L15
P15    .fill 1761607965
A16    .fill L16
P16    .fill 1761607986
A17    .fill L17
P17    .fill 1761608023
