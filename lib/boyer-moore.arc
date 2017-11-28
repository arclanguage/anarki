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

(def rev-match (p s (o j 0))
  ; not sure wether memoization is practical here
  "Checks if 'p' and 's' are identical, and if not then return the mismatch in 's' and its reverse index."
  (if (no (last p)) t
      (is (last p) (last s))
        (rev-match (butlast p) (butlast s) (inc j))
      (list (last s) j))) ; when no match, return character where search failed

(def bad-character (pat)
  "Create a table of safe skips for each character that may cause a mismatch in a substring search."
  (let bc (obj)
      (walk (range 0 255) [= (bc _) (len pat)])
      (walk pat           [= (bc _) (max 1 (pos _ (rev pat)))])
      bc))

(def scan-past (pat in)
  "Returns a list of bytes in 'in' until 'pat' is found. See also [[posmatch]]."
 (time
  (withs (pat   (map int (as cons pat))
          bc    (bad-character pat)
          skip  (len pat)
          look  nil
          sub   nil)
     (accum yield
       (until (is look t)
         (= b (readbytes skip in))
         (= sub (join (cut sub skip) b))
         (= look (rev-match pat sub))
         (walk b yield)
         (when (isnt look t)
               (= skip (- (bc (look 0)) (look 1)))))))))
