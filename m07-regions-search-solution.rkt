;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname m07-regions-search-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require spd/tags)

(@assignment lectures/m07-regions-search)

(@cwl ???) ;replace ??? with your cwl
;;
;; Region and ListOfRegion data definitions provided.  
;;

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

(@problem 1)
#| Use local to encapsulate these templates. |#


(@template-origin encapsulated Region ListOfRegion)

(define (fn-for-region r)
  (local [(define (fn-for-region r)
            (cond [(leaf? r)
                   (... (leaf-label r)
                        (leaf-weight r)
                        (leaf-color r))]
                  [else
                   (... (inner-color r)
                        (fn-for-lor (inner-subs r)))]))

          (define (fn-for-lor lor)
            (cond [(empty? lor) (...)]   
                  [else
                   (... (fn-for-region (first lor))
                        (fn-for-lor (rest lor)))]))]

    (fn-for-region r)))



(@problem 2)
#|

Design a function that consumes a string and a region
and looks for a region with the given label.  If there is one
the function should produce the first one it finds.  If there is
not one it should signal failure by producing false.

Use the encapsulated templates from Problem 1 above.
|#

(@htdf find-region)
(@signature String Region -> Region or false)
;; find region w/ given label
(check-expect (find-region "one" L1) L1)
(check-expect (find-region "one" L2) false)
(check-expect (find-region "one" I4) L1)
(check-expect (find-region "three" I4) L3)

(@template-origin encapsulated Region ListOfRegion try-catch)

(define (find-region l r)
  (local [(define (fn-for-region r)
            (cond [(leaf? r)     
                   (if (string=? l (leaf-label r))
                       r
                       false)]
                  [else
                   (fn-for-lor (inner-subs r))]))

          (define (fn-for-lor lor)
            (cond [(empty? lor) false]
                  [else      
                   (local [(define try (fn-for-region (first lor)))]
                     (if (not (false? try))
                         try
                         (fn-for-lor (rest lor))))]))]
    (fn-for-region r)))