#lang s-exp syntax/module-reader
anarki/lang/module-begin
#:read brackets-read
#:read-syntax brackets-read-syntax

(require (only-in "../brackets.scm"
           [read brackets-read]
           [read-syntax brackets-read-syntax]))
