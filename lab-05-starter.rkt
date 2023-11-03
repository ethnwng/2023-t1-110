;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-05-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; CPSC 110 - Helpers Lab

(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

(@assignment labs/lab-05)

;; If you are:
;;   - A 110 or 107 student replace the first set of '???'s with your cwl.
;;     Remember this, it is what you will do with these @cwl annotations
;;     for the whole course.
;;   - A UBC Extended Learning student, replace the first set of ??? with
;;     the your email address as confirmed in the email you received from
;;     extended learning.  The handin password is also in that email.
;;     Remember this, it is what you will do with these @cwl annotations
;;     for the whole course.
;;   
(@cwl ethnwng ???)

(@problem 1)

;; Problem 1

;; Complete the design of a function called pyramid that takes a natural
;; number n and an image, and constructs an n-tall, n-wide pyramid of
;; copies of that image.

;; For instance, a 3-wide pyramid of cookies would look like this:
;; https://edx-course-spdx-kiczales.s3.amazonaws.com/HTC/lab/cookies-pyramid.png


;; Constants

(define COOKIES
  (bitmap/url
   "https://edx-course-spdx-kiczales.s3.amazonaws.com/HTC/lab/cookies.png"))
(define SQUARE (square 10 "solid" "blue"))


;; ======================================================================
;; Data Definitions

(@htdd Natural)
;; Natural is one of:
;;  - 0
;;  - (add1 Natural)
;; interp. a natural number
(define N0 0)         ;0
(define N1 (add1 N0)) ;1
(define N2 (add1 N1)) ;2

(@dd-template-rules one-of
                    atomic-distinct
                    compound
                    self-ref)

(define (fn-for-natural n)
  (cond [(zero? n) (...)]
        [else
         (... n   ; n is added because it's often useful                   
              (fn-for-natural (sub1 n)))]))


;; ==========
;; Functions

(@htdf pyramid)
(@signature Natural Image -> Image)
;; produce an n-tall, n-wide pyramid of the given image
(check-expect (pyramid 0 COOKIES) empty-image)
(check-expect (pyramid 1 SQUARE) SQUARE)
(check-expect (pyramid 3 COOKIES)
              (above COOKIES
                     (beside COOKIES COOKIES)
                     (beside COOKIES COOKIES COOKIES)))

; (define (pyramid n i) empty-image) ; stub

(@template-origin Natural)
(@template
 (define (pyramid n i)
   (cond [(zero? n) (...)]
         [else
          (... n i  
               (pyramid (sub1 n)))])))
 

(define (pyramid n i)
  (cond [(zero? n) empty-image]
        [else
         (above (pyramid (sub1 n) i)
                (placeBeside n i))]))

(@htdf placeBeside)
(@signature Natural Image -> Image)
;; place images side by side n times
(check-expect (placeBeside 2 COOKIES)
              (beside COOKIES COOKIES))
(check-expect (placeBeside 0 COOKIES)
              empty-image)

; (define (placeBeside n i) empty-image) ;stub

(@template-origin Natural)
(@template
 (define (placeBeside n i)
   (cond [(zero? n) (...)]
         [else
          (... n i  
               (placeBeside (sub1 n)))])))
             
(define (placeBeside n i)
  (cond [(zero? n) empty-image]
        [else
         (beside i (placeBeside (sub1 n) i))]))


;; Problem 2
;;
;; Consider a test tube filled with solid blobs and bubbles.  Over time the
;; solids sink to the bottom of the test tube, and as a consequence the bubbles
;; percolate to the top. 
;;
;; Complete the design of a world program that displays the test tube of blobs.
;; Initially, the test tube is empty, but we can add solid blobs and bubbles by
;; pressing the "s" and "b" keys respectively. Pressing the spacebar will sink
;; each solid blob by one.
;;
;; We have completed most of this program already. All you will need to do is
;; complete the sink function (a helper function for handle-key). This function
;; will consume a list of blobs and sinks each solid blob by one. You can assume
;; that a solid blob will sink past any neighbour just below it.

(@htdw ListOfBlob)
;; CONSTANTS ============================


(define WIDTH 300)
(define HEIGHT 500)


(define TUBE-WIDTH 50)
(define TUBE (overlay/align/offset "middle" "bottom"
                                   (rectangle TUBE-WIDTH
                                              (- HEIGHT TUBE-WIDTH)
                                              "solid"
                                              "silver")
                                   0
                                   (/ TUBE-WIDTH 2)
                                   (circle (/ TUBE-WIDTH 2) "solid" "silver")))

(define MTS (overlay/align "middle" "bottom"
                           TUBE
                           (empty-scene WIDTH HEIGHT)))

(define X-CTR (/ WIDTH 2))

(define BLOB-RADIUS (* 0.4 TUBE-WIDTH))
(define SOLID (circle BLOB-RADIUS "solid" "black"))
(define BUBBLE (circle BLOB-RADIUS "outline" "blue"))


;; DATA DEFINITIONS ====================

(@htdd Blob)
;; Blob is one of:
;; - "solid"
;; - "bubble"
;; interp.  a gelatinous blob, either a solid or a bubble
;; Examples are redundant for enumerations

(@dd-template-rules one-of atomic-distinct atomic-distinct)
(define (fn-for-blob b)
  (cond [(string=? b "solid") (...)]
        [(string=? b "bubble") (...)]))


(@htdd ListOfBlob)
;; ListOfBlob is one of:
;; - empty
;; - (cons Blob ListOfBlob)
;; interp. a sequence of blobs in a test tube, listed from top to bottom.
(define LOB0 empty) ; empty test tube
(define LOB2 (cons "solid" (cons "bubble" empty))) ; solid blob above a bubble

(@dd-template-rules one-of atomic-distinct compound ref self-ref)
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-blob (first lob))
              (fn-for-lob (rest lob)))]))


;; FUNCTIONS ===========================

(@htdf main)
(@signature ListOfBlob -> ListOfBlob)
;; start the world program with (main empty)

(@template-origin htdw-main)
(define (main lob)
  (big-bang lob
    (to-draw render-lob)
    (on-key  handle-key)))



(@htdf render-lob)
(@signature ListOfBlob -> Image)
;; renders the list of blobs on MTS
(check-expect (render-lob empty) MTS)
(check-expect (render-lob (cons "bubble"
                                (cons "solid"
                                      (cons "bubble" empty))))
              (overlay/align "middle" "bottom"
                             (above BUBBLE SOLID BUBBLE)
                             MTS))

(@template-origin fn-composition)
(define (render-lob lob)
  (overlay/align "middle" "bottom"
                 (render-blobs lob)
                 MTS))

(@htdf render-blobs)
(@signature ListOfBlob -> Image)
;; renders the list of blobs in vertical line
(check-expect (render-blobs empty) empty-image)
(check-expect (render-blobs (cons "bubble" (cons "solid" (cons "solid" empty))))
              (above BUBBLE SOLID SOLID))

(@template-origin ListOfBlob)

(@template
 (define (render-blobs lob)
   (cond [(empty? lob) (...)]
         [else
          (... (fn-for-blob (first lob))
               (render-blobs (rest lob)))])))

(define (render-blobs lob)
  (cond [(empty? lob) empty-image]
        [else
         (above (render-blob (first lob))
                (render-blobs (rest lob)))]))

(@htdf render-blob)
(@signature Blob -> Image)
;; produce the image for the given blob
(check-expect (render-blob "bubble") BUBBLE)
(check-expect (render-blob "solid") SOLID)

(@template-origin Blob)

(@template
 (define (render-blob b)
   (cond [(string=? b "solid") (...)]
         [(string=? b "bubble") (...)])))

(define (render-blob b)
  (cond [(string=? b "solid") SOLID]
        [(string=? b "bubble") BUBBLE]))


(@htdf handle-key)
(@signature ListOfBlob KeyEvent -> ListOfBlob)
;; add blobs on "s", "b" and sink on " "
(check-expect (handle-key (cons "bubble" (cons "solid" empty)) "s")
              (cons "solid" (cons "bubble" (cons "solid" empty))))
(check-expect (handle-key (cons "bubble" empty) "b")
              (cons "bubble" (cons "bubble" empty)))
(check-expect (handle-key (cons "solid" (cons "bubble" empty)) " ")
              (cons "bubble" (cons "solid" empty)))

(@template-origin KeyEvent)

(@template   
 (define (handle-key lob ke)
   (cond [(key=? ke " ") (... lob)]
         [else 
          (... lob)])))

(define (handle-key lob ke)
  (cond [(key=? "s" ke) (cons "solid" lob)]
        [(key=? "b" ke) (cons "bubble" lob)]
        [(key=? " " ke) (sink lob)]
        [else lob]))


(@problem 2)
(@htdf sink)
(@signature ListOfBlob -> ListOfBlob)
;; produce a list of blobs that sinks the given solid blobs by one
; !!!
(check-expect (sink empty) empty)
(check-expect (sink (cons "bubble" (cons "solid" (cons "bubble" empty))))
              (cons "bubble" (cons "bubble" (cons "solid" empty))))

(check-expect (sink (cons "solid" (cons "solid" (cons "bubble" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))

(check-expect (sink (cons "solid" (cons "bubble" (cons "bubble" empty))))
              (cons "bubble" (cons "solid" (cons "bubble" empty))))

;; On-Ground Students:
;; As PRE-LAB: Complete these three check-expects, then uncomment them.

;; Online Students:
;; Complete these three check-expects, then uncomment them.

; (check-expect (sink (cons "solid" (cons "bubble" (cons "solid" empty))))
;              (cons "bubble" (cons "solid" (cons "solid" empty))))
; (check-expect (sink (cons "bubble" (cons "solid" (cons "solid" empty))))
;              (cons "bubble" (cons "solid" (cons "solid" empty))))
; (check-expect (sink (cons "solid"
;                          (cons "solid"
;                               (cons "bubble" (cons "bubble" empty)))))
;           (cons "bubble" (cons "bubble" (cons "solid" (cons "solid" empty)))))

; (define (sink lob) lob) ;stub

(@template-origin ListOfBlob)
(@template
 (define (sink lob)
   (cond [(empty? lob) (...)]
         [else
          (... (fn-for-blob (first lob))
               (sink (rest lob)))]))) ;; all solids are at the bottom already


(define (sink lob)
  (cond [(empty? lob) empty]
        [else
         (swap (first lob) (sink (rest lob)))]))
;; all solids are at the bottom already

(@htdf swap)
(@signature Blob ListOfBlob -> ListOfBlob)
;; swap first two items if it goes solid bubble
(check-expect (swap "solid" (cons "bubble" (cons "solid" empty)))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (swap "bubble" (cons "bubble" (cons "solid" empty)))
              (cons "bubble" (cons "bubble" (cons "solid" empty))))

(@template-origin ListOfBlob)
(@template
 (define (swap b lob)
   (cond [(empty? lob) (...)]
         [else
          (...b
           (fn-for-blob (first lob))
           (swap (rest lob)))]))) ;; all solids are at the bottom already

(define (swap b lob)
  (cond [(empty? lob) (cons b empty)]
        [else
         (if (and (string=? b "solid") (string=? "bubble" (first lob)))
             (cons "bubble" (cons "solid" (rest lob)))
             (cons b lob))]))
