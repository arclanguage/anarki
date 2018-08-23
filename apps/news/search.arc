;   Search bar for News
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

(newsop search (q)
  (search-page user q))

(def search-bar (user)
  (tag (form action "search")
    (pr "Search: ")
    (input 'q "" 18 )))

(def search-page (user terms)
  (listpage user (msec) (search (join stories* comments*) tokens.terms) "search"
    (string "Search results for \"" terms #\")))

(def search (stories terms)
  (keep [match-all? _ terms] stories))

(def match-all? (story terms)
  (all idfn (map [match? story _] terms)))

(def match? (story term)
  (some [match-ignoring-case? (string story._) term] '(title url by text)))

(def match-ignoring-case? (s pat)
  (re-match (re:string "(?i:" pat ")")
            s))
