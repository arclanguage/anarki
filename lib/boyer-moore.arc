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
  "Like [[mismatch]] but searching right to left and returning the reverse index."
  (loop (i (- len.s 1))
    (if (isnt pat.i s.i)
      (- len.s i 1)
      (if (> i 0)
        (recur (- i 1))))))

(defmemo bc-table (pat)
  "Create a table of safe shifts for each bad character."
  (ret bc (obj)
      (walk (range 0 255) [= (bc _) (len pat)])
      (walk pat           [= (bc _) (pos _ rev.pat)])))

;todo: implement good suffix rule

(def scan-past (pat in)
  "Returns a list of bytes in 'in' until 'pat' is found."
  (zap [map int (as cons _)] pat)
  (time:with (bc      (bc-table pat)
         buffer  (spliceable-list len.pat))
    (loop (shift len.pat)
      (whenlet b (readbytes shift in)
        (nslide buffer b)
        (awhen (rev-mismatch pat suffix.buffer)
          (recur (- (bc (suffix.buffer (- len.pat it 1)))
                    it)))))
    splice.buffer))
