;   Faster string search using Boyer-Moore-Horspool algorithm
;   Copyright (C) 2017  Pelle Hjek
;
;   This program is free software: you can redistribute it and/or modify
;   it under the terms of the GNU Affero General Public License as published by
;   the Free Software Foundation, either version 3 of the License, or
;   (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU Affero General Public License for more details.
;
;   You should have received a copy of the GNU Affero General Public License
;   along with this program.  If not, see <https://www.gnu.org/licenses/>.

(def rev-mismatch (pat s)
  "Like [[mismatch]] but searching right to left."
  (loop (i (- len.s 1))
    (if (isnt pat.i s.i)
      i
      (if (> i 0)
        (recur (- i 1))))))

(def bc-table (pat)
  "Returns a table of safe shifts for each bad character in 'pat'."
  (ret bc (obj)
      (walk (range 0 255) [= (bc _) (len pat)])
      (walk pat           [= (bc _) (pos _ rev.pat)])))

(def gs-table (pat)
  "Returns a table of safe shifts for each good suffix in 'pat'."
  (ret gs (obj)
     (each i (rev (range 0 (- (len pat) 1)))
           (let p (split pat i)
             (= (gs i)
                (aif
                  (posmatch (rev (p 1)) (rev (p 0)))
                  it 0))))))

(def scan-past (pat in)
"Returns a list of bytes in 'in' until 'pat' is found.
'pat' is read from 'in' but dropped from result.
Not unicode-aware. All chars in 'pat' must be single-byte ASCII.
Returns nil if 'pat' is never found.
(This is mainly for multipart POSTs.)"
  (zap [map int (as cons _)] pat)
  ; First, preprocess 'pat' to make a shift lookup table.
  (time:with (bc     (bc-table pat)
              gs     (gs-table pat)
              buffer (spliceable-list len.pat))
    (loop (shift len.pat)
  ; In first loop iteration, we read a substring of 's' with the same size as 'pat',
      (whenlet b (readbytes shift in)
        (nslide buffer b)
        ; then we compare 'pat' and 'sub' right-to-left.
        (whenlet mm (rev-mismatch pat suffix.buffer)
        ; If 'pat' and the substring do not match, then we need to shift 'pat' within 'in'.
        ; (suffix.buffer is now guaranteed to not be nil)
          (recur
            (max
              ; We shift at very least 1 to the right,
              1
              ; but if the bad character rule permits it, we shift further,
              (- (bc (suffix.buffer mm))
              ; relative to where the mismatch occured
                 (- len.pat mm 1))
              ; and check with the good suffix rule
              (- (gs mm)
                 (- len.pat mm 1))
              ))
          ; if pat and sub match, then return the bytes that have been searched through
          splice.buffer)))))

