#lang rosette
(require "HOVALAAG.rkt")


(define-symbolic inval integer?)

(define 8*IN1-lin
  (list
   (instr #:alu 0 #:a 3 #:b 0 #:c 0 #:d 0 #:w 0 #:f 0 #:pc 0 #:o 0 #:io 0 #:x 0 #:k 0 #:l 0) ; A=IN1
   (instr #:alu 0 #:a 0 #:b 2 #:c 0 #:d 0 #:w 0 #:f 0 #:pc 0 #:o 0 #:io 0 #:x 0 #:k 0 #:l 0) ; B=A
   (instr #:alu 5 #:a 1 #:b 1 #:c 0 #:d 0 #:w 0 #:f 0 #:pc 0 #:o 0 #:io 0 #:x 0 #:k 0 #:l 0) ; A=B=A+B
   (instr #:alu 5 #:a 1 #:b 1 #:c 0 #:d 0 #:w 0 #:f 0 #:pc 0 #:o 0 #:io 0 #:x 0 #:k 0 #:l 0) ; A=B=A+B
   (instr #:alu 5 #:a 0 #:b 0 #:c 0 #:d 0 #:w 1 #:f 0 #:pc 0 #:o 0 #:io 0 #:x 0 #:k 0 #:l 0) ; W=A+B
   (instr #:alu 0 #:a 0 #:b 0 #:c 0 #:d 0 #:w 0 #:f 0 #:pc 0 #:o 1 #:io 0 #:x 0 #:k 0 #:l 0) ; W=A+B
   )
  )



(define sol
  (solve (begin (assert (< inval 5))
                (assert (eq? (run-output
                              8*IN1-lin
                              (reg #:a 0 #:b 0 #:c 0 #:d 0 #:w 0 #:f 0 #:pc 0 #:in1 (list inval) #:in2 null)
                              6
                              )
                             16))
                ))
)

; note: we can't solve for the colck count as the first demo
; as the depth of the program is dependant on the cycle count: we use foldl,
; but the depth must be bounded to pass to the SMT solver.
; so we solve for i*8=16
sol



  