;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname m06-regions-find-region-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment lectures/m06-regions-find-region)

(@cwl ethnwng) ;replace ??? with your cwl

;;
;; Region and ListOfRegion data definitions provided.
;;
(@problem 1)
(@htdd Region ListOfRegion)
(define-struct leaf (label weight color))
(define-struct inner (color subs))
;; Region is one of:
;;  - (make-leaf String Natural Color)
;;  - (make-inner Color ListOfRegion)
;; interp.
;;  an arbitrary-arity tree of regions
;;  leaf regions have label, weight and color
;;  inners have a color and a list of sub-regions
;;
;;  weight is a unitless number indicating how much weight
;;  the given leaf region contributes to whole tree

;; ListOfRegion is one of:
;;  - empty
;;  - (cons Region ListOfRegion)
;; interp. a list of regions

;; All the Ls and Is are Regions
(define L1 (make-leaf "one" 20 "red"))
(define L2 (make-leaf "two" 40 "blue"))
(define L3 (make-leaf "three" 60 "orange"))
(define L4 (make-leaf "four" 30 "black"))
(define L5 (make-leaf "five" 50 "purple"))
(define L6 (make-leaf "six" 80 "yellow"))

(define I1 (make-inner "red"  (list L1 L2 L3)))
(define I2 (make-inner "blue" (list I1 L4)))
(define I3 (make-inner "orange" (list L5 L6)))
(define I4 (make-inner "black" (list I2 I3)))

(define LORE empty)
(define LOR123 (list L1 L2 L3))

(@template-origin Region)

(define (fn-for-region r)
  (cond [(leaf? r)
         (... (leaf-label r)
              (leaf-weight r)
              (leaf-color r))]
        [else
         (... (inner-color r)
              (fn-for-lor (inner-subs r)))]))

(@template-origin ListOfRegion)

(define (fn-for-lor lor)
  (cond [(empty? lor) (...)]
        [else
         (... (fn-for-region (first lor))
              (fn-for-lor (rest lor)))]))






;; Design a function that consumes a string and a region and looks for a region
;; with the given label.  If there is one the function should produce the first
;; one it finds.  If there is not one it should signal failure by producing
;; false.
;;
;; To save time the starter includes @htdf, @signature, purpose, stub and
;; check-expects.  You need to complete the last two check-expects, and then
;; go on to @template-origin, @template, and the final function definitions.
;; But be sure you are able to do the provided steps yourself. You will need to
;; do them on later problems, and of course problem sets, labs, and exams.
;;
;; Remember, trust the natural recursionS, and think about contributions and
;; combinations.
;;

(@htdf find-region--region find-region--lor)
(@signature String Region -> Region or false); will produce a region, or fail
(@signature String ListOfRegion  -> Region or false)
;; find region w/ given label, or fail
(check-expect (find-region--lor "one" empty) false)
(check-expect (find-region--region "one" L1) L1)
(check-expect (find-region--region "one" L2) false)
(check-expect (find-region--lor "two" LOR123) L2)
(check-expect (find-region--region "three" I4) L3)

; (define (find-region--region label r) false)
; (define (find-region--lor label lor) false)


(@template-origin Region)

(define (find-region--region s r)
  (cond [(leaf? r)
         (if (string=? (leaf-label r) s)
             r
             false)]
        [else
         (find-region--lor s (inner-subs r))]))

(@template-origin ListOfRegion try-catch)

(define (find-region--lor s lor)
  (cond [(empty? lor) false]
        [else
         (if (not (false? (find-region--region s (first lor))))
             (find-region--region s (first lor))
             (find-region--lor s (rest lor)))]))
             ;in first - Region or false
             ;in rest - Region or false

;; NEW RULE:
;; if TWO CASES ARE TRUE
;; - you are calling a function that might fail (false)
;; - and you want to do something if it does
;; use try-catch template