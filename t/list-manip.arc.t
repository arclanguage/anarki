;;; basic.arc.t - test the various ways of declaring and executing functions.
;;;
;;; This file is licensed under the MIT X11 License:
;;; http://www.opensource.org/licenses/mit-license.php
;;;
;;; (C) Copyright by Shlomi Fish, 2008

(load "arctap.arc")

(plan 1)

; TEST
(test-iso (map [+ 5 _] '(10 20 100)) '(15 25 105) "Simple Map")
