#lang racket
(require racket/list)
(require rebellion/type/record)
; HOVALAAG Processor emulator

; define the instruction type: implicitly 32b
(define-record-type instr (alu a b c d w f pc o io x k l))

; register type
; A is a 12-bit generic register
; B is a 12-bit generic register
; C is a 12-bit counting register
; D is a 12-bit data storage register
; W is a 12-bit buffer register - cannot loop back into CPU
; F is a 1bit branch reg
; The 8-bit PC register stores the program counter
; 69 (nice) bits of state total, ignoring io12 (lists)
(define-record-type reg (a b c d w f pc in1 in2))

; L allways K optional
(define (K?L I)
  ; the K and L registers can be combined to form K:L if X is set
  (if (eq? (instr-x I) 1)
    (+ (arithmetic-shift (instr-k I) 8) (instr-l I))
    (instr-l I)
  )
)

; same as above, but for K allways L optional
(define (KL? I)
  ; the K and L registers can be combined to form K:L if X is set
  (if (eq? (instr-x I) 1)
    (+ (arithmetic-shift (instr-k I) 8) (instr-l I))
    (instr-k I)
  )
)

(define (ALU I S)
  ; The ALU is the only functional unit.
  ; probably the most sensible way to model this is to
  ; define functions for each reg that determine it's next
  ; state (state') wrt state, instr.
  (match (instr-alu I)
    [0   0]
    [1  (- (reg-a S))]
    [2  (reg-b S)]
    [3  (reg-c S)]
    [4  (arithmetic-shift (reg-a S) 1)]
    [5  (+ (reg-a S) (reg-b S))]
    [6  (- (reg-b S) (reg-a S))]
    [7  (+ (reg-a S) (reg-b S) (reg-f S))]
    [8  (- (reg-a S) (reg-b S) (reg-f S))]
    [9  (bitwise-ior (reg-a S) (reg-b S))]
    [10 (bitwise-and (reg-a S) (reg-b S))]
    [11 (bitwise-xor (reg-a S) (reg-b S))]
    [12 'bcd-add-invalid]
    [13 'bcd-sub-invalid]
    [14 'bcd-addc-invalid]
  )
)

; functions with
(define (A I S)
  ; new value of A given instr, state
  (match (instr-a I)
    [0 (reg-a S)]
    [1 (ALU I S)]
    [2 (reg-d S)]
    [3 (car (if (instr-io 0) (reg-in1 S) (reg-in2 S)))] ; read from io 1 or 2 depending on io flag
  )
)

(define (B I S)
  ; new value of B given instr, state
  (match (instr-b I)
    [0 (reg-b S)]
    [1 (ALU I S)]
    [2 (reg-a S)] ; enables single-cycle inplace swap
    [3 (KL? I)] ; can load immediate
  )
)

(define (C I S)
  ; C has a branch calculation as well
  (match (instr-c I)
    [0 (reg-c S)]
    [1 (ALU I S)]
    [2 (sub1 (reg-c S))]
    [3 (sub1 (reg-c S))] ; Also impl in the PC calculation
  )
)

(define (D I S)
  ; D is a swap reg for A
  (match (instr-d I)
    [0 (reg-d S)]
    [1 (reg-a S)]
  )
)

(define (W I S)
  (match (instr-w I)
    [0 (reg-w S)]
    [1 (ALU I S)]
    [2 (reg-a S)]
    [3 (KL? I)]
  )
)

(define (F I S)
  (match (instr-f I)
    [0 (reg-f S)]
    [1 (if (eq? (ALU I S) 0) 1
           0)]
    [2 (if (negative? (ALU I S) 0) 1
          0)]
    [3 (if (positive? (ALU I S) 0) 1
          0)]
  )
)

(define (PC I S)
  (if (and (eq? (instr-c I) 3) (> (reg-c S) 1))
    ; if C is DECNZ, jump to L
    (instr-l I)
    ; else, read our instruction
    (match (instr-pc I)
      [0 (add1 (reg-pc S))]
      [1 (instr-l I)]
      ; jump if true

      [2 (if (eq? (reg-f S) 1)
           (K?L I)
           (add1 (reg-pc S)))] ; TODO: fix K:L
      ; jump if false
      [3 (if (eq? (reg-f S) 0)
           (K?L I)
           (add1 (reg-pc S)))] ; TODO: fix K:L
      )
  )
)

(define (IO1 I S)
  (if (and
        ; we want to read a value
        (eq? instr-a 3)
        ; from IO1
        (eq? (instr-io I) 0)
        ; and there is somthing to read
        (pair? (reg-in1 S))
      )
      ; if all true, new IO1 drops that element
      (cdr (reg-in1 S))
    ; nothing read - same as before
    (reg-in1 S)
  )
)

(define (IO2 I S)
  (if (and
        ; we want to read a value
        (eq? instr-a 3)
        ; from IO1
        (eq? (instr-io I) 1)
        ; and there is somthing to read(require racket/list)
        (pair? (reg-in2 S))
      )
      ; if all true, new IO1 drops that element
      (cdr (reg-in2 S))
    ; nothing read - same as before
    (reg-in2 S)
  )
)


; run a cycle by getting the next processor state
; returns pair (new state, output value1, output value 2)
; output values are 0 if not written to... unclear what the true behaviour is
(define (apply I S)
  (reg
    #:a (A I S)
    #:b (B I S)
    #:c (C I S)
    #:d (D I S)
    #:w (W I S)
    #:f (F I S)
    #:pc (PC I S)
    #:in1 (IO1 I S)
    #:in2 (IO2 I S)
  )
)

(define (w0 I S)
  (if (and (eq? (instr-o I) 1) (eq? (instr-o I) 0))
    (reg-w S)
    0
  )
)

(define (w1 I S)
  (if (and (eq? (instr-o I) 1) (eq? (instr-o I) 1))
    (reg-w S)
    0
  )
)

(define state-zeros (reg #:a 0 #:b 0 #:c 0 #:d 0 #:w 0 #:f 0 #:pc 0 #:in1 null #:in2 null))
(define instr-zero (instr #:alu 0 #:a 0 #:b 0 #:c 0 #:d 0 #:w 0 #:f 0 #:pc 0 #:o 0 #:io 0 #:x 0 #:k 0 #:l 0))
(define instr-swap (instr
                    #:alu 2 ; ALU reg is B
                    #:a 1 ; A loaded from ALU
                    #:b 2 ; A loaded from A
                    #:c 0 #:d 0 #:w 0 #:f 0 #:pc 0 #:o 0 #:io 0 #:x 0 #:k 0 #:l 0))


(define (cycle cycle program-S-W0-W1)
  ; uses clock to apply I to S, appending to W0 and W1
  ; returns a new triple [S W0 W1]
  (define program (first program-S-W0-W1))
  (define S (second program-S-W0-W1))
  (define pc (+ (reg-pc S) 1) )
  (define I (last (take program pc)))
  (define W0 (third program-S-W0-W1))
  (define W1 (fourth program-S-W0-W1))

  (list program ; technically, can do self-modifing code...
        (apply I S)
        (list (w0 I S) W0)
        (list (w1 I S) W1)
  )
)

(define (run program initial-state cycles)
  (foldl
    cycle ; proc
    (list program initial-state null null) ; init reg, w0, w1
    (build-list cycles values) ; n cycles, indexed
  )
)


(run
  (list instr-swap instr-swap instr-swap)
  (reg #:a 1 #:b 2 #:c 0 #:d 0 #:w 0 #:f 0 #:pc 0 #:in1 null #:in2 null)
  2
)
