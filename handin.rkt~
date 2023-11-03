;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname mt1-p5-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment exams/2023w1-mt1/mt1-p5)

(@cwl ethnwng)   ;fill in your CWL here (same as for problem sets)

(@problem 1) ;do not edit or delete this line
(@problem 2) ;do not edit or delete this line
(@problem 3) ;do not edit or delete this line
(@problem 4) ;do not edit or delete this line
(@problem 5) ;do not edit or delete this line

(@htdd ListOfNumber)
;; ListOfNumber is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; interp. a list of numbers
(define LON1 empty)
(define LON2 (cons 2 (cons 5 empty)))

(@dd-template-rules one-of             
                    atomic-distinct   
                    compound          
                    self-ref)      

(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

#|

Design a function that consumes a list of numbers. The function should
compute the product of all odd numbers in the given list. For example:

(multiply-odds (cons 3 (cons 1 (cons 2 (cons 4 (cons 7 empty))))))

should produce 21, which is 3 * 1 * 7

Your function design must include an @htdf tag, @signature tag, purpose,
commented out stub, appropriate tests, a @template-origin tag, a @template
tag, and a function definition. You must follow all applicable design rules.

This problem will be autograded.  NOTE that all of the following are required.
Violating one or more will cause your solution to receive 0 marks.

  - Files must not have any errors when the Check Syntax button is pressed.
    Press Check Syntax and Run often, and correct any errors early.

  - You must define a function with the same name as in the @htdf tag below.

|#

(@htdf multiply-odds) ;uncomment this line when you start
(@signature ListOfNumber -> Number)
;; produce the product of all odd numbers in a list
(check-expect (multiply-odds empty) 1)
(check-expect (multiply-odds (cons 5 empty)) 5)
(check-expect (multiply-odds (cons 2 (cons 5 (cons 3 empty))))
              (* 5 3))
(check-expect (multiply-odds (cons 2 (cons 4 empty)))
              0)
(check-expect (multiply-odds (cons 3 (cons 1 (cons 2 (cons 4 (cons 7 empty))))))
              (* 3 1 7))
(check-expect (multiply-odds (cons 1 (cons 2 (cons 3 empty)))) 3)

(check-expect (multiply-odds (cons 4 (cons 4 (cons 4 (cons 4 empty))))) 0)

; (define (multiply-odds lon) 0) ;stub

(@template-origin ListOfNumber)
(@template
 (define (multiply-odds lon)
   (cond [(empty? lon) (...)]
         [else
          (... (first lon)
               (multiply-odds (rest lon)))])))

(define (multiply-odds lon)
  (cond [(empty? lon) 1]
        [else
         (if (containsOdd? lon)
             (if (odd? (first lon))
                 (* (first lon) (multiply-odds (rest lon)))
                 (multiply-odds (rest lon)))
             0)]))

;; assuming that a list with no odd numbers returns zero instead of 1.
;; and that empty returns 1 always.

#;
(define (multiply-odds lon)
  (cond [(empty? lon) 1]
        [else
         (if (odd? (first lon))
             (* (first lon) (multiply-odds (rest lon)))
             (multiply-odds (rest lon)))]))

;; multiply-odds function that does not use a helper

(@htdf containsOdd?)
(@signature ListOfNumber -> Boolean)
;; produce true if there exists one odd number in the list
(check-expect (containsOdd? (cons 3 empty)) true)
(check-expect (containsOdd? empty) false)
(check-expect (containsOdd? (cons 2 empty)) false)

; (define (containsOdds? lon) false) ;stub


(@template-origin ListOfNumber)
(@template
 (define (containsOdd? lon)
   (cond [(empty? lon) (...)]
         [else
          (... (first lon)
               (containsOdd? (rest lon)))])))

(define (containsOdd? lon)
  (cond [(empty? lon) false]
        [else
         (if (odd? (first lon))
             true
             (containsOdd? (rest lon)))]))
