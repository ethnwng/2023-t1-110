;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname lab-06-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(@assignment labs/lab-06)

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
;; DATA DEFINITIONS ===============
(@htdd SentenceTree ListOfSentenceTree)
(define-struct stree (prefix subs))
;; SentenceTre is (make-stree String ListOfSentenceTree
;; interp. SentenceTree is an abitrarily wide and long tree
;; each node in the tree contains a String with a prefix

;; ListOfSentenceTree is one-of:
;; - empty
;; - (cons SentenceTree ListOfSentenceTree)
(define PERCHED (make-stree "PERCHED ON THE TIP OF A SINKING SHIP" empty))
(define IN-A (make-stree "IN A BACK TO SCHOOL SPECIAL ABOUT MONO" empty))
(define MY (make-stree "MY FAVOURITE SONG ON REPEAT" empty))
(define FREEZE (make-stree "FREEZE TIME" empty))
(define WE (make-stree "WE ARE" (list IN-A PERCHED)))
(define YOU (make-stree "YOU" empty))
(define TO (make-stree "TO" (list FREEZE MY)))
(define LIKE (make-stree "LIKE" (list YOU WE)))
(define JOKING (make-stree "JOKING ABOUT JEALOUSY" empty))
(define KISS (make-stree "KISS ME" (list JOKING LIKE TO)))

(define ST1 (make-stree "KISS ME" (list JOKING LIKE TO)))

(define LOST1 empty)
(define LOST2 (list JOKING LIKE TO))
(define LOST3 (list LIKE WE IN-A))

(@template-origin SentenceTree)

(define (fn-for-sentencetree st)
  (... (stree-prefix st)
       (fn-for-lost (stree-subs st))))

(@template-origin ListOfSentenceTree)

(define (fn-for-lost lost)
  (cond [(empty? lost) (...)]
        [else
         (... (fn-for-sentencetree (first lost))
              (fn-for-lost (rest lost)))]))


(define TEXT-SIZE 10)
(define TEXT-COLOUR "black")

;; FUNCTIONS ======================
(@problem 2)
(@htdf sentence-count--sentencetree sentence-count--lost)
(@signature SentenceTree -> Natural)
(@signature ListOfSentenceTree -> Natural)
;; produce the sum of all possible sentences in a tree 
(check-expect (sentence-count--sentencetree KISS) 6)
(check-expect (sentence-count--lost empty) 0)
(check-expect (sentence-count--lost (list MY)) 1)
(check-expect (sentence-count--sentencetree TO) 2)
(check-expect (sentence-count--sentencetree LIKE) 3)

;stubs
; (define (sentence-count--sentencetree st) 0)
; (define (sentence-count--lost lost) 0) 

(@template-origin SentenceTree)

(define (sentence-count--sentencetree st)
  (if (empty? (stree-subs st))
      1
      (sentence-count--lost (stree-subs st))))

(@template-origin ListOfSentenceTree)

(define (sentence-count--lost lost)
  (cond [(empty? lost) 0]
        [else
         (+ (sentence-count--sentencetree (first lost))
            (sentence-count--lost (rest lost)))]))


(@problem 3)
(@htdf render--stree render--lost)
(@signature SentenceTree -> Image)
(@signature ListOfSentenceTree -> Image)
;; produce the rendering of the tree
(check-expect (render--stree WE)
              (beside (text "WE ARE" TEXT-SIZE TEXT-COLOUR)
           (above/align "left"(text
                    "IN A BACK TO SCHOOL SPECIAL ABOUT MONO"
                    TEXT-SIZE TEXT-COLOUR)        
           (text"PERCHED ON THE TIP OF A SINKING SHIP"
                    TEXT-SIZE TEXT-COLOUR))))

(check-expect (render--lost (list FREEZE MY))
              (above/align "left" (text
                               "FREEZE TIME"
                               TEXT-SIZE TEXT-COLOUR)
                       (text "MY FAVOURITE SONG ON REPEAT"
                             TEXT-SIZE TEXT-COLOUR)))
(check-expect (render--stree TO)                               
(beside (text "TO" TEXT-SIZE TEXT-COLOUR)
          (above/align "left" (text
                               "FREEZE TIME"
                               TEXT-SIZE TEXT-COLOUR)
                       (text "MY FAVOURITE SONG ON REPEAT"
                             TEXT-SIZE TEXT-COLOUR))))
              
; (define (render--sentencetree st) empty-image)
; (define (render--lost lost) empty-image)

(@template-origin SentenceTree)
#;
(define (render--sentencetree st)
  (beside (text (stree-prefix st) TEXT-SIZE TEXT-COLOUR)
          (render--lost (stree-subs st))))

(@template-origin ListOfSentenceTree)
#;
(define (render--lost lost)
  (cond [(empty? lost) empty-image]
        [else
         (above/align "left" (render--sentencetree (first lost))
                      (render--lost (rest lost)))]))


(define (render--stree st)
  (beside (text (stree-prefix st) TEXT-SIZE TEXT-COLOUR)
           (render--lost (stree-subs st))))

(define (render--lost lost)
  (cond [(empty? lost) empty-image]
        [else
          (above/align "left" (render--stree (first lost))
   	                      (render--lost (rest lost)))]))