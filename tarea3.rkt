#lang racket

; P1
; Implementar la función recursiva primo? que determine si el valor del argumento dado como un número 
; natural es un número primo. Un número primo es un número natural que solo tiene dos factores que son 
; el número mismo y el uno. Los números 0 y 1 no se consideran primos. 

(define (primo? n)
  (define (f n c)
    (cond
      [(equal? n 0) #f]
      [(equal? n 1) #f]
      [(< n (* c c)) #t]
      [(zero? (modulo n c)) #f]
      [else (f n (add1 c))]
    )
  )
  (f n 2)
)

(primo? 1)
(primo? 4)
(primo? 17)

; P2
; Implementar la función recursiva sumdpar que regrese la suma de los dígitos pares que conforman a un 
; número entero no negativo que se le pasa como argumento.

(define (sumdpar n)
  (define (f n c)
    (cond 
      [(equal? n 1) c]
      [(and (< n 10) (even? n)) (+ c n)]
      [(even? (modulo n 10)) (f (truncate (/ n 10)) (+ c (modulo n 10)))]
      [else (f (truncate (/ n 10)) c)]
    )
  )
  (f n 0)
)

(sumdpar 0)
(sumdpar 8032)
(sumdpar 174)

; P5
; Implementar la función recursiva bitor que calcule el or lógico entre bits dados  como elementos  de 
; 2 listas dadas como sus argumentos. Asumir que las dos  listas son del mismo tamaño.

(define (bitor l1 l2)
  (cond
    [(and (null? l1) (null? l2)) '()] 
    [(equal? (+ (car l1) (car l2)) 0) (cons 0 (bitor (cdr l1) (cdr l2)))]
    [(> (+ (car l1) (car l2)) 0) (cons 1 (bitor (cdr l1) (cdr l2)))]
  )
)

(bitor '() '())
(bitor '(1 0 1) '(0 1 1))
(bitor '(1 0 1 0) '(1 0 1 0))

