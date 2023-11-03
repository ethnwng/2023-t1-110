;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname m08-abstract-fold-definition-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR 
;; PARTNER.
;;
(require spd/tags)
(@assignment lectures/m08-abstract-fold-definition)
(@cwl ethnwng) ;replace ??? with your cwl

(@htdd Course)
(define-struct course (number credits dependents))
;; Course is (make-course Natural Natural (listof Course))
;; interp. a course with a course number,
;;         the number of credits the course is worth,
;;         a list of courses that have this course as a pre-requisite

(define LOC0 empty)
(define C322 (make-course 322 3 LOC0))
(define C320 (make-course 320 3 LOC0))
(define C319 (make-course 319 4 LOC0))
(define C317 (make-course 317 3 LOC0))
(define C314 (make-course 314 3 LOC0))
(define C313 (make-course 313 3 LOC0))
(define C312 (make-course 312 3 LOC0))
(define C311 (make-course 311 3 LOC0))
(define LOC1 (list C319))
(define C310 (make-course 310 4 LOC1))
(define C304 (make-course 304 3 LOC0))
(define C302 (make-course 302 3 LOC0))
(define C303 (make-course 303 3 LOC0))
(define LOC2 (list C304 C313 C314 C317 C320 C322))
(define C221 (make-course 221 4 LOC2))
(define LOC3 (list C313 C317))
(define C213 (make-course 213 4 LOC3))
(define LOC4 (list C213 C221 C310 C311 C312))
(define C210 (make-course 210 4 LOC4))
(define C203 (make-course 203 3 LOC0))
(define C189 (make-course 189 1 LOC0))
(define LOC5 (list C189 C203 C210 C302 C303))
(define C110 (make-course 110 4 LOC5))
(define C100 (make-course 100 3 LOC0))

(@template-origin Course (listof Course) encapsulated)

(define (fn-for-course c)
  (local [(define (fn-for-course c)
            (... (course-number c)
                 (course-credits c)
                 (fn-for-loc (course-dependents c))))

          (define (fn-for-loc loc)
            (cond [(empty? loc) (...)]
                  [else
                   (... (fn-for-course (first loc))
                        (fn-for-loc (rest loc)))]))]

    (fn-for-course c)))


(@problem 1)
;;
;; Design an abstract function called foldr2 based on the (listof X) template.
;; Work backwards through the HtDF recipe starting from the fn definition.
;;
(@htdf foldr2)
;(@signature __ __ __ -> __)3 inputs maybe one output
;(@signature __ __ (listof__) -> __)first and rest are used there must be a list
;(@signature (__ __ -> __) __ (listof__) -> __)
;(@signature (X __ -> __) __ (listof X) -> __)operates on the list 
;(@signature (X __ -> Y) Y (listof X) -> Y)
(@signature (X Y -> Y) Y (listof X) -> Y)
;; abstract fold for (listof X)
; COPY TEST copies given input 
(check-expect (foldr2 cons empty (list 1 2 3)) (list 1 2 3))
(check-expect (foldr2 cons empty (cons 1 (cons 2 (cons 3 empty))))
              (cons 1 (cons 2 (cons 3 empty))))
; COUNT TEST number of elements in the list

(check-expect (local [(define (one x rnr) (add1 rnr))]
                (foldr2 one 0 (list "a" "b")))
              2)
; count test we created a function that takes two parameters but only uses one
; so that we in essence are removing the (first lox)

#;
(define (foldr2 lox)
  (cond [(empty? lox) (...)]
        [else
         (... (first lox)
              (foldr2 (rest lox)))]))



(@template-origin (listof X))

(define (foldr2 c1 b1 lox)
  (cond [(empty? lox) b1]
        [else
         (c1 (first lox)
             (foldr2 c1 b1 (rest lox)))]))

;; we introduce a parameter for each set of dots

;; check expects copy the function and use it as scratch work

#;
; COPY TEST
(define (foldr2 c1 b1 lox)
  (cond [(empty? lox) empty]
        [else
         (cons (first lox)
               (foldr2 c1 b1 (rest lox)))]))
#;
; COUNT TEST
(define (foldr2 c1 b1 lox)
  (cond [(empty? lox) 0]
        [else
         (add1 ;(first lox)
          (foldr2 c1 b1 (rest lox)))]))
; need local to remove the (first lox)
; cannot edit the function

(@problem 2)
#|

The first step in designing an abstract fold function is always the same -
copy the encapsulated templates, and add one parameter for each set of ...

By convention the parameters that stand for functions go first, the parameters
that stand for base values go after that, and the data always goes last. We
have done this first step for you below.

Complete the design of the following abstract fold function for Course.
Note that we have already given you the actual function definition and the
template tag. You must complete the design with a signature, purpose,
function definition and the two following check-expects:

  - uses the fold function to produce a copy of C110
  - uses the fold function to count the total number of courses,
    in the C110 tree, including C110


|#

(@htdf fold-course)
;(@signature __ __ __ __ -> __)
;(@signature (__ __ __ -> __) (__ __ -> Y) __  -> Y)
(@signature (Natural Natural Y -> X) (X Y -> Y) Y Course -> X)
;; abstract fold for Course
; COPY TEST
; what c1 c2 b1 return the input?
(check-expect (fold-course make-course cons empty  C110)
              C110)
; COUNT TEST
; 20 items in the tree?
(check-expect (local [(define (c1 num cred rmr) (add1 rmr))]
                (fold-course c1 + 0 C110)) 20)
              

(@template-origin Course (listof Course) encapsulated)

(define (fold-course c1 c2 b1 c0)
  (local [(define (fn-for-course c) ; -> X
            (c1 (course-number c)
                (course-credits c)
                (fn-for-loc (course-dependents c))))

          (define (fn-for-loc loc) ; -> Y
            (cond [(empty? loc) b1]
                  [else
                   (c2 (fn-for-course (first loc))
                       (fn-for-loc (rest loc)))]))]

    (fn-for-course c0)))

