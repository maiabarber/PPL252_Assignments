#lang racket

(provide (all-defined-out))

(define zip
  (lambda (xs ys)
    (if (or (empty-lzl? xs) (empty-lzl? ys))
        empty-lzl
        (cons-lzl (cons (head xs) (head ys))
                  (lambda () (zip (tail xs) (tail ys)))))))

(define integers-from
  (lambda (n)
    (cons-lzl n (lambda () (integers-from (+ n 1))))))

(define cons-lzl cons)
(define empty-lzl? empty?)
(define empty-lzl '())
(define head car)
(define tail
  (lambda (lzl)
    ((cdr lzl))))

(define leaf? (lambda (x) (not (list? x))))

;; Signature: map-lzl(f, lz)
;; Type: [[T1 -> T2] * Lzl(T1) -> Lzl(T2)]
(define map-lzl
  (lambda (f lzl)
    (if (empty-lzl? lzl)
        lzl
        (cons-lzl (f (head lzl))
                  (lambda () (map-lzl f (tail lzl)))))))

;; Signature: take(lz-lst,n)
;; Type: [LzL*Number -> List]
;; If n > length(lz-lst) then the result is lz-lst as a List
(define take
  (lambda (lz-lst n)
    (if (or (= n 0) (empty-lzl? lz-lst))
      empty-lzl
      (cons (head lz-lst)
            (take (tail lz-lst) (- n 1))))))

; Signature: nth(lz-lst,n)
;; Type: [LzL*Number -> T]
;; Pre-condition: n < length(lz-lst)
(define nth
  (lambda (lz-lst n)
    (if (= n 0)
        (head lz-lst)
        (nth (tail lz-lst) (- n 1)))))


;;; Q3.1
; Signature: append$(lst1, lst2, cont) 
; Type: [List * List * [List -> T]] -> T
; Purpose: Returns the concatination of the given two lists, with cont pre-processing
(define append$
  (lambda (lst1 lst2 cont)
    (let loop ((l lst1) (k cont))
      (if (empty? l)
          (k lst2)
          (loop (cdr l)
                (lambda (v) (k (cons (car l) v))))))
  )
)

;;; Q3.2
; Signature: equal-trees$(tree1, tree2, succ, fail) 
; Type: [Tree * Tree * [Tree ->T1] * [Pair->T2] ]-> T1 U T2
; Purpose: Determines the structure identity of a given two lists, with post-processing succ/fail
(define equal-trees$
  (lambda (t1 t2 succ fail)
    (cond
      ;; שני עלים (אטומיים)
      [(and (leaf? t1) (leaf? t2))
       (succ (cons t1 t2))]

      ;; שני עצים שהם רשימות
      [(and (list? t1) (list? t2))
       (let loop ((xs t1) (ys t2) (acc '()))
         (cond
           ;; הצלחה - שני העצים הסתיימו
           [(and (null? xs) (null? ys)) (succ (reverse acc))]

           ;; אורך שונה - מחזיר את מה שנשאר
           [(null? xs) (fail (cons '() ys))]
           [(null? ys) (fail (cons xs '()))]

           ;; רקורסיה על זוגות איברים
           [else
            (equal-trees$
             (car xs) (car ys)
             ;; הצלחה חלקית – ממשיכים בלולאה
             (lambda (v) (loop (cdr xs) (cdr ys) (cons v acc)))
             ;; כישלון – מיד מחזירים
             fail)]))]

      ;; טיפוסים שונים לגמרי => הבדל מבני
      [else (fail (cons t1 t2))])))



;;; Q4.1

;; Signature: as-real(x)
;; Type: [ Number -> Lzl(Number) ]
;; Purpose: Convert a rational number to its form as a
;; constant real number
(define (as-real x)
  (letrec ([next (lambda () (cons-lzl x next))])
    (cons-lzl x next)))



;; Signature: ++(x, y)
;; Type: [ Lzl(Number) * Lzl(Number) -> Lzl(Number) ]
;; Purpose: Addition of real numbers
(define ++
  (lambda (x y)
    (cons-lzl (+ (head x) (head y))
              (lambda () (++ (tail x) (tail y))))))

;; Signature: --(x, y)
;; Type: [ Lzl(Number) * Lzl(Number) -> Lzl(Number) ]
;; Purpose: Subtraction of real numbers
(define --
  (lambda (x y)
    (cons-lzl (- (head x) (head y))
              (lambda () (-- (tail x) (tail y))))
  )
)

;; Signature: **(x, y)
;; Type: [ Lzl(Number) * Lzl(Number) -> Lzl(Number) ]
;; Purpose: Multiplication of real numbers
(define **
  (lambda (x y)
    (cons-lzl (* (head x) (head y))
              (lambda () (** (tail x) (tail y))))
  )
)

;; Signature: //(x, y)
;; Type: [ Lzl(Number) * Lzl(Number) -> Lzl(Number) ]
;; Purpose: Division of real numbers
(define //
  (lambda (x y)
    (cons-lzl (/ (head x) (head y))
              (lambda () (// (tail x) (tail y))))
  )
)

;;; Q4.2.a
;; Signature: sqrt-with(x y)
;; Type: [ Lzl(Number) * Lzl(Number) -> Lzl(Lzl(Number)) ]
;; Purpose: Using an initial approximation `y`, return a 
;; sequence of real numbers which converges into the 
;; square root of `x`
(define sqrt-with
  (lambda (x y)
    (define next
      (lambda (yₙ)
        (map-lzl
         (lambda (pair)
           (/ (+ (car pair) (/ (cdr pair) (car pair))) 2))
         (zip yₙ x))))
    
    (define recur
      (lambda (yₙ)
        (cons-lzl yₙ (lambda () (recur (next yₙ))))))
    
    (recur y)
  )
)

;;; Q4.2.b
;; Signature: diag(lzl)
;; Type: [ Lzl(Lzl(T)) -> Lzl(T) ]
;; Purpose: Diagonalize an infinite lazy list
(define diag
  (lambda (lzl)
    (cons-lzl (nth (head lzl) 0) ; x11
              (lambda () (diag (map-lzl tail (tail lzl)))))
  )
)

;;; Q4.2.c
;; Signature: rsqrt(x)
;; Type: [ Lzl(Number) -> Lzl(Number) ]
;; Purpose: Take a real number and return its square root
;; Example: (take (rsqrt (as-real 4.0)) 6) => '(4.0 2.5 2.05 2.0006097560975613 2.0000000929222947 2.000000000000002)
(define rsqrt
  (lambda (x)
(diag (sqrt-with x x))))
