;;; basic.arc.t - test the various ways of declaring and executing functions.
;;;
;;; This file is licensed under the MIT X11 License:
;;; http://www.opensource.org/licenses/mit-license.php
;;;
;;; (C) Copyright by Shlomi Fish, 2008

(load "arctap.arc")

(plan 9)

; TEST
(test-iso (map [+ 5 _] '(10 20 100)) '(15 25 105) "Simple Map")

; TEST
(test-iso (+ (list "Hello" 5 6) (list 100 200) (list "Lambda"))
          (list "Hello" 5 6 100 200 "Lambda")
          "List Contactenation")

; Testing element lookup.
(with (l (list 1 "Twenty Four" 'fox "Handclap" 90))
      ; TEST
      (test-is (l 0) 1 "nth 0")
      ; TEST
      (test-is (l 1) "Twenty Four" "nth 1")
      ; TEST
      (test-is (l 2) 'fox "nth 2")
      ; TEST
      (test-is (l 3) "Handclap" "nth 3")
      ; TEST
      (test-is (l 4) 90 "nth 4")
      ; TEST
      (test-is ((list 10 20 30) 1) 20 "Testing dynamic nth"))

(with (l (list 1 20 303 444 5.5))
      (= (cadr l) "Two")
      ; TEST
      (test-iso l (list 1 "Two" 303 444 5.5)
                "Testing (= (cadr..."))
      
