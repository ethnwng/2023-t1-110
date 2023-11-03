;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mt1-p4-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment exams/2023w1-mt1/mt1-p4)

(@cwl ethnwng)   ;fill in your CWL here (same as for problem sets)

(@problem 1) ;do not edit or delete this line
(@problem 2) ;do not edit or delete this line
(@problem 3) ;do not edit or delete this line
(@problem 4) ;do not edit or delete this line

#|
 Carefully consider the following data definitions.
|#

(@htdd Gift)

(define-struct ball (radius))
(define-struct block (height width length))
;; Gift is one of:
;;  - (make-ball Natural)
;;  - (make-block Natural Natural Natural)
;; interp.
;;   A gift. A ball has a radius in centimeters. A block has width,
;;   height and length all in centimeters.
;; CONSTRAINT: all measurements > 0

(define G1 (make-ball 2))
(define G2 (make-block 3 4 5))

(@dd-template-rules one-of
                    compound)

(define (fn-for-gift g)
  (cond [(ball? g) (... (ball-radius g))]
        [(block? g) (... (block-width g)
                         (block-height g)
                         (block-length g))]))


(@htdd Package)

(define-struct package (to contents))
;; Package is (make-package String Gift)
;; interp. a package with who it is for and a single enclosed gift

(define P1 (make-package "Gregor" G1))
(define P2 (make-package "Emily"  G2))

(@dd-template-rules compound ref)

(define (fn-for-package p)
  (... (package-to p)
       (fn-for-gift (package-contents p))))



(@htdd Dimensions)

(define-struct dims (w h l))
;; Dimensions is (make-dims Number Number Number)
;; interp. (w)idth, (l)ength and (h)eight of a box, all in centimeters
;; CONSTRAINT: all dimensions > 0

(define D1 (make-dims 1 2 3))

(@dd-template-rules compound)

(define (fn-for-dims d)
  (... (dims-w d)
       (dims-h d)
       (dims-l d)))


#|

 Complete the design of the following function that produces the required
 internal dimensions of a package. The required dimensions of a package
 depend on the gift it contains. For each kind of gift, the required 
 dimensions are
  - for a ball   (2 * radius) for all three of width, length and height
  - for a block  the block's own dimensions

So, for example:

   (package-required-dims (make-package "C" (make-block 2 3 4)))

should produce
              
   (make-dims 2 3 4)

Your function design must include an @htdf tag, @signature tag, purpose,
commented out stub, appropriate tests, a @template-origin tag, a @template
tag, and a function definition. You must follow all applicable design rules.

This problem will be autograded.  NOTE that all of the following are required.
Violating one or more will cause your solution to receive 0 marks.

 - You must not edit, comment out, or delete the existing @htdf tag.

 - You must not edit, comment out, or delete the existing @signature tag.

 - You must not edit, or delete the existing purpose statement.

 - You must not edit, comment out, or delete the existing test, but you
   should add additional tests AFTER the existing test.

 - You should comment out, but not delete the existing stub.

 - Files must not have any errors when the Check Syntax button is pressed.
   Press Check Syntax and Run often, and correct any errors early.

|#

(@htdf package-required-dims) ;uncomment this when you start
(@signature Package -> Dimensions)
;; return the required width length and height for each specific package
(check-expect (package-required-dims (make-package "C" (make-block 2 3 4)))
              (make-dims 2 3 4))
(check-expect (package-required-dims (make-package "B" (make-ball 2)))
              (make-dims (* 2 2) (* 2 2) (* 2 2)))
(check-expect (package-required-dims (make-package "A" (make-ball 1)))
              (make-dims 2 2 2))
(check-expect (package-required-dims (make-package "D" (make-block 1 2 3)))
              (make-dims 1 2 3))
(check-expect (package-required-dims (make-package "E" (make-block 2 2 2)))
              (make-dims 2 2 2))
(check-expect (package-required-dims (make-package "F" (make-block 5 4 5)))
              (make-dims 5 4 5))
(check-expect (package-required-dims (make-package "G" (make-ball 5)))
              (make-dims 10 10 10))
(check-expect (package-required-dims (make-package "H" (make-ball 9)))
              (make-dims 18 18 18))
(check-expect (package-required-dims (make-package "I" (make-block 2 4 2)))
              (make-dims 2 4 2))

; (define (package-required-dims p) D1) ;stub

(@template-origin Package)
(@template
 (define (package-required-dims p)
   (... (package-to p)
        (fn-for-gift (package-contents p)))))

(define (package-required-dims p)
  (size (package-contents p)))

(@htdf size)
(@signature Gift -> Dimensions)
;; give the correct dimensions for the given gift
(check-expect (size (make-ball 2))
              (make-dims (* 2 2) (* 2 2) (* 2 2)))
(check-expect (size (make-block 2 3 4))
              (make-dims 2 3 4))

; (define (size g) D1) ;stub

(@template-origin Gift)
(@template
 (define (size g)
   (cond [(ball? g) (... (ball-radius g))]
         [(block? g) (... (block-width g)
                          (block-height g)
                          (block-length g))])))

(define (size g)
  (cond [(ball? g) (make-dims (* 2 (ball-radius g))
                              (* 2 (ball-radius g))
                              (* 2 (ball-radius g)))]
        [(block? g) (make-dims (block-height g)
                               (block-width g)
                               (block-length g))]))

;; struct of block and dimensions have different param order.