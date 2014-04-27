(suite strings
       (suite lines
              empty-string (assert-same '("")
                                        (lines ""))
              enpty-lines (assert-same  '("" "")
                                        (lines "\n"))
              single-line (assert-same '("a b")
                                       (lines "a b"))
              trailing-newline (assert-same '("a b" "")
                                            (lines "a b\n"))
              skip-returns (assert-same '("a b" "")
                                        (lines "a b\r\n"))
              skip-returns-without-newline (assert-same '("a b")
                                                        (lines "a b\r"))
              all-together (assert-same '("a b" "c d" "" "e f")
                                        (lines "a b\nc d\n\ne f")))


       (suite urlencode
              passes-alphanumerics-through (assert-same "abc"
                                                        (urlencode "abc"))
              escapes-spaces (assert-same "a%20b"
                                          (urlencode "a b"))
              escapes-other-punctuation (assert-same "a%40b%23%25%5edef"
                                                     (urlencode "a@@b#%^def"))
              escapes-unicode (assert-same "abc%c3%bcd"
                                           (urlencode "abcüd"))
              escapes-url-params (assert-same "item%3fid%3d1"
                                              (urlencode "item?id=1")))

       (suite subst
              substitutes-one-occurrence-of-single-char (assert-same "mbc"
                                                                     (subst "a" "m" "abc"))
              substitutes-all-occurrences-of-single-char (assert-same "mbcmbd"
                                                                      (subst "a" "m" "abcabd"))
              substitutes-all-occurrences-of-multi-char-pattern (assert-same "mcmd"
                                                                             (subst "ab" "m" "abcabd")))

       (suite posmatch
              handles-equal-character (assert-same 0
                                                   (posmatch "a" "a"))
              handles-inequal-character (assert-same nil
                                                     (posmatch "a" "b"))
              handles-single-character-patterns-at-start (assert-same 0
                                                                      (posmatch "a" "abc"))
              handles-single-character-pattern-in-middle (assert-same 1
                                                                      (posmatch "b" "abc"))
              handles-single-character-pattern-at-end (assert-same 2
                                                                   (posmatch "c" "abc"))
              handles-missing-single-character-pattern (assert-same nil
                                                                    (posmatch "d" "abc"))
              handles-patterns-at-start (assert-same 0
                                                     (posmatch "ab" "abcd"))
              handles-pattern-in-middle (assert-same 1
                                                     (posmatch "bc" "abcd"))
              handles-pattern-at-end (assert-same 2
                                                  (posmatch "cd" "abcd"))
              handles-missing-pattern (assert-same nil
                                                   (posmatch "de" "abcd"))
              can-take-an-optional-start-index (assert-same 3
                                                            (posmatch "a" "banana" 2))
              can-take-a-list-of-chars-instead-of-a-substring (assert-same 1
                                                                           (posmatch '(#\a #\b) '(#\c #\a #\b)))
              can-take-a-predicate-instead-of-a-substring (assert-same 3
                                                                       (posmatch (fn (c) (in c #\a #\b)) "foobar")))

       (suite tokens
              splits-at-whitespace (assert-same '("abc" "def" "I'm" "too" "lazy")
                                                (tokens "abc def I'm too lazy"))
              splits-at-other-char (assert-same '("b" "n" "n")
                                                (tokens "banana" #\a)))


       (suite positions
              1 (assert-same '(3 7 11 15)
                             (positions #\space "abc def I'm too lazy"))
              2 (assert-same '(2 5 7)
                             (positions #\a "That abacus"))
              with-predicate (assert-same '(0 3 4)
                                          (positions odd '(1 2 4 5 7))))

       (suite nonascii
              fails-on-all-ascii (assert-nil (nonascii "Abc"))
              passes-on-some-unicode (assert-t (nonascii "bcΓ")))

       (suite headmatch
              matches-at-head (assert-t (headmatch "abc" "abcde"))
              fails-elsewhere (assert-nil (headmatch "abc" "xabcde"))
              passes-at-explicitly-provided-index (assert-t (headmatch "abc" "xabcde" 1))
              works-with-lists-of-chars (assert-t (headmatch '(#\a #\b #\c) "abcde"))
              matches-lists-against-lists (assert-t (headmatch '(#\a #\b) '(#\a #\b #\c))))

       (suite endmatch
              passes-at-end (assert-t (endmatch "cde" "abcde"))
              fails-elsewhere (assert-nil (endmatch "abc" "abcde"))
              works-with-lists-of-chars (assert-t (endmatch '(#\c #\d #\e) "abcde")))

       (suite rev
              works-with-strings (assert-same "cba"
                                              (rev "abc")))

       (suite subst
              substitutes-all-found-patterns (assert-same "catbard dogbard"
                                                          (subst "foo" "bar" "catfood dogfood"))
              can-substitute-any-value-with-a-printed-representation (assert-same "b(1 2)n(1 2)n(1 2)"
                                                                                  (subst "a" '(1 2) "banana")))

       (suite multisubst
              substitutes-multiple-patterns-at-once (assert-same "B1n1n1"
                                                                 (multisubst '(("a" 1) ("b" "B")) "banana")))

       (suite trim
              drops-whitespace-from-end (assert-same " abc"
                                                     (trim " abc " 'end))
              drops-whitespace-from-start-and-end (assert-same "abc"
                                                               (trim " abc " 'both))
              can-drop-arbitrary-characters (assert-same "bc"
                                                         (trim "aabcaa" 'both #\a))
              can-drop-based-on-a-predicate (assert-same "c"
                                                         (trim "aabcaa" 'both (fn (_) (in _ #\a #\b)))))

       (suite num
              converts-numbers-to-strings (assert-same "123"
                                                       (num 123))
              inserts-a-comma-every-three-digits (assert-same "1,234,567"
                                                              (num 1234567))
              handles-negative-numbers (assert-same "-123,456"
                                                    (num -123456))
              can-take-an-optional-precision (assert-same "1.23"
                                                          (num 1.2345 2))
              can-pad-zeroes-to-the-right (assert-same "1.2000"
                                                       (num 1.2 4 t))
              can-add-a-leading-zero (assert-same "0.3000"
                                                  (num 0.3 4 t t)))

       (suite pluralize
              0-pants (assert-same "pants"
                                   (pluralize 0 "pant"))
              1-pant (assert-same "pant"
                                  (pluralize 1 "pant"))
              2-pants (assert-same "pants"
                                   (pluralize 2 "pant"))
              0-oxen (assert-same "oxen"
                                  (pluralize 0 "ox" "oxen"))
              1-explosion (assert-same "explosion"
                                       (pluralize 1 "explosion" "EXPLOSIONS ARE AWESOME!"))
              7-formulae (assert-same "formulae"
                                      (pluralize 7 "formula" "formulae")))


       (suite plural
              0-pants (assert-same "0 pants"
                                   (plural 0 "pant"))
              1-pant (assert-same "1 pant"
                                  (plural 1 "pant"))
              2-pants (assert-same "2 pants"
                                   (plural 2 "pant"))
              0-oxen (assert-same "0 oxen"
                                  (plural 0 "ox" "oxen"))
              1-explosion (assert-same "1 explosion"
                                       (plural 1 "explosion" "EXPLOSIONS ARE AWESOME!"))
              7-formulae (assert-same "7 formulae"
                                      (plural 7 "formula" "formulae"))))
