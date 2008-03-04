;;; macros1.arc.t - test some macros features.
;;;
;;; This file is licensed under the MIT X11 License:
;;; http://www.opensource.org/licenses/mit-license.php
;;;
;;; (C) Copyright by Shlomi Fish, 2008

(load "arctap.arc")

(plan 2)

(mac += (var . values)
    `(= ,var (+ ,var ,@values)))

(with (a 5)
    (+= a 10)
    ; TEST
    (ok (is a 15) "+= works")
    (+= a 100 200 300)
    ; TEST
    (ok (is a 615) "+= (list) works"))

