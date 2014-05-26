(let calls 0
  (def-lru foo (x) 2  ; cache capacity of 2
    ++.calls
    (+ x 1))
  (def num-foo-calls ()
    calls)
  (def reset-foo-calls ()
    (= calls 0)))

; insert first item
(test-iso "lru-cache calls a function to compute a result"
  3
  (foo 2))

(test-iso "first call computes function"
  1
  (num-foo-calls))

(reset-foo-calls)
(test-iso "lru-cache caches result"
  '(3 0)
  (let result foo.2
    (list result (num-foo-calls))))

; insert second item
(foo 3)
(reset-foo-calls)
(test-iso "lru-cache caches multiple results"
  '(3 4 0)
  (with (result2 foo.2
         result3 foo.3)
    (list result2 result3 (num-foo-calls))))

; make 2 most recent
(foo 2)
; now evict 3
(foo 4)
(reset-foo-calls)
(test-iso "evicts least recently used call"
  '(4 1)
  (let result foo.3
    (list result (num-foo-calls))))

(test-iso "repeated identical calls yield identical results"
  '(4 4)
  (do
    foo.3
    foo.4
    (list foo.3 foo.3)))
