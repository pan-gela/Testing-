;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hof-mountains) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))



;; 4a
(check-expect (min-max < (list 4 2 5 3 6 4)) (list 2 6))
(check-expect (min-max string<? (list "bean" "cod" "ape" "dice")) (list "ape" "dice"))

;; min-max: (X X -> Bool) (listof X) -> (list X X)
(define (min-max comp lst)
  (list (foldl (lambda (x y) (cond [(comp x y) x]
                                   [else y])) (first lst) (rest lst))
        (foldl (lambda (x y) (cond [(comp x y) y]
                                   [else x])) (first lst) (rest lst))))


;; 4b
(check-expect (in-order? < (list -1 0 1 2 3 4 5)) true)
(check-expect (in-order? string<? (list "apple" "bag" "cat" "ear" "dog" "hi")) false)
(check-expect (in-order? < (list 1 0 1 2 3 4 5)) false)


(check-expect (in-order? < (list 0 1 2 3 5 135)) true)
(check-expect (in-order? < (list 0 1 2 3 5 135 -1)) false)
(check-expect (in-order?
               (lambda (str-a str-b) (< (string-length str-a)
                                        (string-length str-b)))
               (list "A" "short" "wordlist" "for y'all"))
              true)


;; in-order? (X X -> Bool) (listof X) -> Bool
(define (in-order?  comp lst)
  (list? (foldl (lambda (x y) (cond [(boolean? y) false]
                                    [(comp (first y) x) (cons x y)]
                                    [else false]))
                (cons (first lst) empty) (rest lst))))


;; 4c
(check-expect (slice 0 0 (list 'a 1 1)) (list 'a))
(check-expect (slice 2 4 (list 0 1 2 3 4 5 6)) (list 2 3 4))
(check-expect (slice 0 1 (list 'a 'b 'c)) (list 'a 'b))

;; slice: Nat Nat (listof X) -> (listof X)
;; Requires: a >= b, a and b < length of lst
(define (slice a b lst)
  (first
   (foldl (lambda (x y)
            (cond [(and (<= (second y) b)
                        (>= (second y) a))
                   (list (foldr cons (list x) (first y))
                         (add1 (second y)))]
                  [else (list (first y) (add1 (second y)))]))
          (list empty 0) lst)))


;; 4d
(check-expect (split-n 3 (list 1 2 3 4 5 6 7 8 9 10 11 12))
(list (list 1 4 7 10) (list 2 5 8 11) (list 3 6 9 12)))
(check-expect (split-n 5 (list 1 2 3 4 5 6 7 8 9 10 11 12))
              (list (list 1 6 11) (list 2 7 12) (list 3 8) (list 4 9)
                    (list 5 10)))
(define my-lst (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
(check-expect (split-n 3 my-lst)
              (list (list 1 4 7 10 13)
                    (list 2 5 8 11 14)
                    (list 3 6 9 12 15)))
(check-expect (split-n 4 (list '1 '2 '3 '4 '5 '6 '7 '8 '9 '10 '11 '12 '13 '14 '15))
              (list (list '1 '5 '9 '13) (list '2 '6 '10 '14)
                    (list '3 '7 '11 '15) (list '4 '8 '12)))
(check-expect (split-n 5 my-lst)
              (list (list 1 6 11) (list 2 7 12) (list 3 8 13)
                    (list 4 9 14) (list 5 10 15)))
(check-expect (split-n 6 (list '1 '2 '3 '4 '5 '6 '7 '8 '9 '10 '11 '12 '13 '14 '15))
              (list (list '1 '7 '13) (list '2 '8 '14) (list '3 '9 '15)
                    (list '4 '10) (list '5 '11) (list '6 '12)))

;; split-n: Nat (listof X) -> (listof (listof X))
(define (split-n n lst)
  (foldl (lambda (og new)
           (foldr cons
            (list
             (first (foldl (lambda (ref newlst)
                    (cond [(zero? (modulo (second newlst) n))
                           (list (foldr cons (list ref) (first newlst))
                                 (add1 (second newlst)))]
                          [else (list (first newlst)
                                      (add1 (second newlst)))]))
                 (list empty 0) og)))
            new))
         empty
         (build-list n
                     (lambda (current)
                       (foldr (lambda (nlst newlst)
                                (cond [(<= (length newlst) (- (length lst) current)) newlst]
                                      [else (rest newlst)]))
                              lst lst)))))
