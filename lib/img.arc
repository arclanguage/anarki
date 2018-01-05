;   Image processing
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

($:require racket/draw racket/class)

(def scale (in out scale-x scale-y (o type 'png))
  (withs
      (source      ($ (read-bitmap in))
       src-width   ($ (send source get-width))
       src-height  ($ (send source get-height))
       dest-width  (int (* src-width scale-x))
       dest-height (int (* src-height scale-y))
       dest        ($ (make-object bitmap% dest-width dest-height))
       dc          ($ (new bitmap-dc% (bitmap target))))
    ($ (send dc scale x y))
    ($ (send dc draw-bitmap-section source 0 0 0 0 src-width src-height))
    ($ (send dest save-file out type)))
    out)

(def resize (in out dest-width dest-height (o type 'png))
  (withs
      (source     ($ (read-bitmap in))
       src-width  ($ (send source get-width))
       src-height ($ (send source get-height))
       dest       ($ (make-object bitmap% dest-width dest-height))
       dc         ($ (new bitmap-dc% (bitmap dest))))
    ($ (send dc draw-bitmap-section-smooth source
                                           0 0 dest-width dest-height
                                           0 0 src-width  src-height))
    ($ (send dest save-file out type)))
    out)
