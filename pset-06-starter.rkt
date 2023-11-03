;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname pset-06-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR 
;; PARTNER.
;;
(require spd/tags)

(@assignment psets/pset-06); Do not edit or remove this tag

;; If you are:
;;   - A 110 or 107 student replace the first set of '???'s with your cwl.
;;     For problem sets, If you have a partner, please replace the second
;;     set of '???'s with their cwl.  Remember this, it is what you will
;;     do with these @cwl annotations for the whole course.
;;   - A UBC Extended Learning student, replace the first set of ??? with
;;     your email address as confirmed in the email you received from
;;     extended learning.  The handin password is also in that email.
;;     Remember this, it is what you will do with these @cwl annotations
;;     for the whole course.
;;   
(@cwl ethnwng ???)


(@problem 1)
;; Below is the start of a data definition called Course that represents limited
;; information about UBC courses.  Below there are only two example data.  
;; Please complete this definition by adding constants C110, C213, C313 and C317
;; which are representations of the descendent tree for 110, 213, 313 and 317.  
;; You can find the information you need at
;;  https://cs110.students.cs.ubc.ca/psets/pset-06-image.png
;;
;; NOTE 1: Use the information in the image above, rather than any other source.
;;         We are significantly simplying the information.
;;
;; NOTE 2: Do this very carefully, the autograder wants to see correct results
;;         from the functions you design to operate on this data.
;;
;; NOTE 3: The tree you will make for C110 will be a bit odd because 210 has 110
;;         as a pre-req, and both 213 and 221 have 210 as a pre-req, and313 has
;;         213 AND 221 as a pre-req, and 317 has 213 AND 221 as a pre-req. As a
;;         result, 313 and 317 will both show up twice in your descendent tree
;          for C110. This is okay for this problem set.
;; NOTE 4: Expect this step of the problem set to take you some time.


(@htdd Course ListOfCourse)
(define-struct course (number credits dependents))
;; Course is (make-course Natural Natural ListOfCourse)
;; interp. a course with a course number,
;;         the number of credits the course is worth, and a
;;         list of courses that list this course as a direct pre-requisite

;; ListOfCourse is one of:
;; - empty
;; - (cons Course ListOfCourse)
;; interp. a list of courses

(define LOC0 empty)
(define C100 (make-course 100 3 LOC0))
(define C189 (make-course 189 1 LOC0))
(define C203 (make-course 203 3 LOC0))
(define C304 (make-course 304 3 LOC0))
(define C319 (make-course 319 4 LOC0))
(define C310 (make-course 310 4 (list C319)))
(define C311 (make-course 311 3 LOC0))
(define C312 (make-course 312 3 LOC0))

(define C313 (make-course 313 3 LOC0))
(define C317 (make-course 317 3 LOC0))

(define C314 (make-course 314 3 LOC0))
(define C320 (make-course 320 3 LOC0))
(define C322 (make-course 322 3 LOC0))
(define C302 (make-course 302 3 LOC0))
(define C303 (make-course 303 3 LOC0))

(define C221 (make-course 221 4 (list C304 C313 C314 C317 C320 C322)))

(define C213 (make-course 213 4 (list C313 C317)))

(define C210 (make-course 210 4 (list C213 C221 C304 C310 C311 C312)))

(define C110 (make-course 110 4 (list C189 C203 C210 C302 C303)))

(define (fn-for-course c)
  (... (course-number c)
       (course-credits c)
       (fn-for-loc (course-dependents c))))

(define (fn-for-loc loc)
  (cond [(empty? loc) (...)]
        [else
         (... (fn-for-course (first loc))
              (fn-for-loc (rest loc)))]))


(@problem 2)
;;
;; Design a function that produces the list of all the course numbers in the
;; course's tree including the given course's number.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;

(@htdf show-courses--course show-courses--loc)
(@signature Course -> ListOfNatural)
(@signature ListOfCourse -> ListOfNatural)
;; produce a list of all course numbers in a courses tree
(check-expect (show-courses--loc empty) empty)
(check-expect (show-courses--course C317)
              (list 317))
(check-expect (show-courses--loc (list C317))
              (list 317))
(check-expect (show-courses--course C213)
              (list 213 313 317))
#;
(define (show-courses--course c) empty)
#;
(define (show-courses--loc loc) empty)

(@template-origin Course)
(define (show-courses--course c)
  (cons (course-number c) (show-courses--loc (course-dependents c))))

(@template-origin ListOfCourse)
(define (show-courses--loc loc)
  (cond [(empty? loc) empty]
        [else
         (append (show-courses--course (first loc))
                 (show-courses--loc (rest loc)))]))

(@problem 3)
;;
;; Design a function that takes two arguments: a Course and a Natural, in that
;; order. It produces the list of courses in the tree that are worth that
;; many credits or more.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;
(@htdf equal-credits--course equal-credits--loc)
(@signature Course Natural -> ListOfCourse)
(@signature ListOfCourse Natural -> ListOfCourse)
;; produce a list of courses that are worth more or equal to the credits
(check-expect (equal-credits--course C189 1) (list C189))
(check-expect (equal-credits--loc (list C100 C189 C319) 3)
              (list C100 C319))
(check-expect (equal-credits--course C203 4) empty)
(check-expect (equal-credits--course C213 1) (list C213 C313 C317))
(check-expect (equal-credits--loc (list C213) 4) (list C213))
(check-expect (equal-credits--loc empty 5) empty)

#;
(define (equal-credits--course c credits) empty)
#;
(define (equal-credits--loc loc credits) empty)

(@template-origin Course)
(define (equal-credits--course c credits)
  (if (>= (course-credits c) credits) 
      (cons c (equal-credits--loc (course-dependents c) credits))
      (equal-credits--loc (course-dependents c) credits)))

(@template-origin ListOfCourse)
(define (equal-credits--loc loc credits)
  (cond [(empty? loc) empty]
        [else
         (append (equal-credits--course (first loc) credits)
                 (equal-credits--loc (rest loc) credits))]))

(@problem 4)
;;
;; Design a function that produces the largest course number in the tree.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;
(@htdf largest-course-number--course largest-course-number--loc)
(@signature Course -> Natural)
(@signature ListOfCourse -> Natural)
;; produce the largest course number from the tree
(check-expect (largest-course-number--course C312) 312)
(check-expect (largest-course-number--course C213) 317)
(check-expect (largest-course-number--loc (list C317 C313 C312)) 317)
(check-expect (largest-course-number--loc empty) 0)

#;
(define (largest-course-number--course c) 0)
#;
(define (largest-course-number--loc loc) 0)

(@template-origin Course)
(define (largest-course-number--course c)
  (if (> (course-number c) (largest-course-number--loc (course-dependents c)))
      (course-number c)
      (largest-course-number--loc (course-dependents c))))

(@template-origin ListOfCourse)
(define (largest-course-number--loc loc)
  (cond [(empty? loc) 0]
        [else
         (if (> (largest-course-number--course (first loc))
                (largest-course-number--loc (rest loc)))
             (largest-course-number--course (first loc))
             (largest-course-number--loc (rest loc)))]))

(@problem 5)
;;
;; Design a function that takes two arguments: a Course and a Natural, in that
;; order. It produces the course in the tree with that course number. If it
;; can't find a course in the given tree with that course number, it signals
;; failure by producing false.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;

(@htdf find-course--course find-course--loc)
(@signature Course Natural -> Course or false)
(@signature ListOfCourse Natural -> Course or false)
;; finds the course in the tree, or returns false
(check-expect (find-course--course C100 100) C100)
(check-expect (find-course--course C100 110) false)
(check-expect (find-course--course C213 313) C313)
(check-expect (find-course--course C213 110) false)
(check-expect (find-course--loc empty 110) false)
(check-expect (find-course--loc (list C313 C317) 317) C317)

#;
(define (find-course--course c c-code) c)
#;
(define (find-course--loc loc c-code) (first loc))

(@template-origin Course)
(define (find-course--course c c-code)
  (if (= c-code (course-number c))
      c
      (find-course--loc (course-dependents c) c-code)))

(@template-origin ListOfCourse try-catch)
(define (find-course--loc loc c-code)
  (cond [(empty? loc) false]
        [else
         (if (not (false? (find-course--course (first loc) c-code)))
             (find-course--course (first loc) c-code)
             (find-course--loc (rest loc) c-code))]))