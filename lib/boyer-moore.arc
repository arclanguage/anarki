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
           (bc    (bad-character pat)
           sub    nil
           revpat (rev pat))
      ; TODO: find better way of not including the boundary, because this is really slow -- perhaps using spliceable-lists
     ([cut _ 0 (- (len _) (len pat))] ; remove boundary
     (flat (accum yield
       (loop (skip (len pat))
         (withs
             (b      (readbytes skip in)
              sub    (join (cut sub skip) b)
              revsub (rev sub))
           (yield b)
           (aif (mismatch revpat revsub)
              (if (< it (len sub))
                (recur (- (bc (revsub it)) it))))))))))))
