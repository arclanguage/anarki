;   Search bar for news.arc
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

(def search-bar (user)
  (aform
    (fn (req)
      (search-page user (arg req "term")))
    (single-input "" 'term 20 "Search")))

(def search-page (user term)
  (listpage user (msec) (search stories* term) "search"
    (string "Search results for " term)))

(def search (stories term)
  (keep [match? _ term] stories))

(def match? (story term)
  (some [match-ignoring-case? (string story._) term] '(title url by)))

(def match-ignoring-case? (s pat)
  (re-match (re:string "(?i:" pat ")")
            s))
