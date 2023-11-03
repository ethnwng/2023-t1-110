;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Untitled) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(@htdd SentenceTree ListOfSentenceTree)
(define-struct stree (prefix subs))
;; SentenceTree is (make-stree String ListOfSentenceTree)
;; interp. An arbitrary wide and long tree
;; SentenceTree nodes contain a String which is a prefix

;; ListOfSentenceTree is one-of:
;; - empty
;; - (cons SentenceTree ListOfSentenceTree)
(define STREE1  (make-stree "KISS ME" (list STREE2 STREE3 STREE4)))
(define STREE2  (make-stree "JOKING ABOUT JEALOUSY" empty))
(define STREE3  (make-stree "LIKE" (list STREE5 STREE6)))
(define STREE4  (make-stree "TO" (list STREE7 STREE8)))
(define STREE5  (make-stree "YOU REALLY MEAN IT" empty))
(define STREE6  (make-stree "WE ARE" (list STREE9 STREE10)))
(define STREE7  (make-stree "FREEZE TIME" empty))
(define STREE8  (make-stree "MY FAVOURITE SONG ON REPEAT" empty))
(define STREE9  (make-stree "IN A BACK TO SCHOOL SPECIAL ABOUT MONO" empty))
(define STREE10 (make-stree "PERCHED ON THE TIP OF A SINKING SHIP" empty))

(define LOST1 empty)
(define LOST2 (list STREE2 STREE3 STREE4))

(@template-origin SentenceTree)
(define (fn-for-sentencetree st)
  (... (prefix st)
       (fn-for-lost subs)))

(@template-origin ListOfSentenceTree)
(define (fn-for-lost lost)
  (cond [(empty? lost) (...)]
        [else
         (... (fn-for-sentencetree (first lost))
              (fn-for-lost (rest lost)))]))