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
  "Return the last byte of 's' and its reverse index where 'pat' doesn't match. See also [[mismatch]]."
  (zap rev pat)
  (zap rev s)
  (loop (patc (pop pat)
         sc   (pop s)
         i  0)
      (if sc
        (if (is patc sc)
            (recur (pop pat) (pop s) (inc i))
            (list sc i))
        nil)))

(defmemo bad-character (pat)
  "Create a table of safe skips for each character that may cause a mismatch in a search."
  (ret bc (obj)
      (walk (range 0 255) [= (bc _) (len pat)])
      (walk pat           [= (bc _) (pos _ (rev pat))])))

;TODO: implement the good-suffix rule

(def scan-past (pat in)
  "Returns a list of bytes in 'in' until 'pat' is found."
  (zap [map int (as cons _)] pat)
  (time (with
           (bc (bad-character pat)
           sub nil)
     (flat (accum yield
       (loop (skip (len pat))
         (withs
             (b   (readbytes skip in)
              sub (join (cut sub skip) b))
           (yield b)
           (aif (rev-mismatch pat sub)
             (recur (- (bc (it 0)) (it 1)))))))))))
