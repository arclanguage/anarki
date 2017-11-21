;   An event calendar
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

($ (require racket/date))

(= events-title* "My calendar"
   events-url*   "http://example.com"
   events-desc*  "What kind of events are posted here.")

(= eventdir* (+ srvdir* "events/")
   event-maxid* 0
   events* nil)

(def event (id)
  (keep [is _!id id] events*))

(deftem event  id      nil
               by      nil
               title   nil
               address nil
               url     nil
               date    nil)

(= months
  '(nil "Jan" "Feb" "Mar" "Apr" "May" "Jun"
        "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(def weekday (d)
  ('("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
  ($.date-week-day ($.seconds->date (toseconds d)))))

(def till (d)
  (tostring
    (let days (roundup (- (- (days-since (toseconds d)) 0.5)))
      (if (< days 7)
              (if (is days 0) (pr "today")
                  (is days 1) (pr "tomorrow")
                  (+ "in " (plural days "day")))
          (< days 30)
              (pr "in " (plural (round (/ days 7)) "week"))
          (< days 365)
              (pr "in " (plural (round (/ days 30)) "month"))
          t
              (pr "in " (plural (round (/ days 365)) "year"))))))

(def toseconds (d)
  (let _ 0 ;unused fields
     ($.date->seconds
       ($.date _ _ _ (int (d 2)) (int (d 1)) (int (d 0)) _ _ _ _))))

(def earlier (d1 d2)
  (apply < (map toseconds (list d1 d2))))

(def later (d1 d2)
  (apply > (map toseconds (list d1 d2))))

(def future (d)
  (earlier (date) d))

(def past (d)
  (later (date) d))

(def upcoming (events)
  (rem [past _!date]
    (sort (fn (e1 e2) (earlier e1!date e2!date))
       events)))

(defop events req
  (let user (get-user req)
    (events-page user)))

(def events-page (user)
  (ensure-events)
  (whitepage
    (tab
      (center
        (tag b (link  events-title* events-url*))
        (br2)
        (w/bars
          (link "new event" "new-event")
          (link "rss" "events-rss")))
      (tag ul
        (each event (upcoming events*)
          (display-event user event))))))

(def load-events ()
   (= events* nil)
   (each id (map int (dir eventdir*))
      (do
      (= event-maxid* (max event-maxid* id))
    (push (temload 'event (string eventdir* id)) events*)))
   events*)

(def ensure-events ()
  (ensure-dir eventdir*)
  (if (no events*)
      (load-events)))

(defop new-event req
  (let user (get-user req)
    (if
      (no user)
        (login-page
          "You need to be logged in to post events."
          (fn (user ip) (new-event-page user)))
      (new-event-page user))))

(def edit-event (user event)
  (if (may-edit user event)
    (new-event-page
      user
      event!title
      event!address
      event!url
      event!date
      event!id)
      "Not allowed."))

(def new-event-page
    (user
    (o title)
    (o address)
    (o url)
    (o d (date))
    (o id)
    (o msg))
  (whitepage
    (pagemessage msg)
    (tab
      (uform user req
        (process-event
          (get-user req)
          (arg req "t")
          (arg req "a")
          (arg req "u")
          (list
            (arg req "year")
            (string (pos (arg req "month") months))
            (arg req "day"))
          id)
        (row 'Title   (input 't title 40))
        (row 'Address (input 'a address 40))
        (row 'Link    (input 'u url 40))
        (row 'Date
          (tag span
            (menu  'year   (range ((date) 0) (+ ((date) 0) 10)) (int (d 0)))
            (menu  'month  (map [months _] (range 1 12)) (months (int (d 1))))
            (menu  'day    (range 1 31) (int (d 2)))))
        (row (submit))))))

(def process-event (user title address url d id)
  (if
      (no (errsafe:toseconds d))
        (new-event-page user title address url d id
                       "Please pick a valid date.")
      (past d)
        (new-event-page user title address url d id
                       "Please pick a future date.")
      (blank title)
        (new-event-page user title address url d id
                       "Please enter a title.")
      (nor (valid-url url) (blank url))
        (new-event-page user title address url d id
                       "Please enter a valid link (or no link).")
      (do
        (add-event user title address url d id)
        (events-page user))))

(def add-event (user title address url date (o id))
  (ensure-events)
  (let event   (inst 'event
      'id      (if (no id) (++ event-maxid*) id)
      'by      user
      'title   title
      'address address
      'url     url
      'date    date)
    (save-event event)
    (if id (pull [is _!id id] events*))
    (push event events*)))

(def save-event (event) (temstore 'event event (string eventdir* event!id)))

(def openstreetmap (address)
  (string "https://nominatim.openstreetmap.org/search?q="
          (urlencode address)))

(def format-title (title url)
  ([if (nonblank url)
    (link _ url) _] (ellipsize title)))

(def format-address (address)
  (if (nonblank address)
      (link (ellipsize address)
            (openstreetmap address)) ""))

(def format-date (d)
  (string (weekday d) " "
          (d 2) " "
          (months (int (d 1))) " "
          (check (int (d 0)) [isnt _ ((date) 0)] "")))

(def display-event (user event)
  (tag li
    (tag span
      (tag b (pr (format-title event!title event!url)))
      (br)
      (tag span
        (w/bars
          (pr (format-date event!date))
          (pr (till event!date))
          (pr (format-address event!address))
          (when (may-edit user event)
            (w/bars
              (onlink "edit" (edit-event user event))
              (onlink "delete" (del-confirm-event user event)))))))
    (br2)))

(def may-edit (user event)
  (and user
       (or (is event!by user)
           (admin user))))

(def del-confirm-event (user event)
  (whitepage
    (tab
     (row (display-event user event))
     (spacerow 20)
     (row (uform user req
       (if (is (arg req "b") "Yes")
         (delete-event user event)
         (events-page user))
       (pr "Delete this event? ")
       (but "Yes" "b") (sp) (but "No"))))))

(def delete-event (user event)
  (if (may-edit user event)
      (do
        (pull [is event _] events*)
        (aif (file-exists (string eventdir* event!id)) (rmfile it))
        (events-page user))
      (pr "Not allowed.")))

(defop events-rss ()
  (ensure-events)
  (rss-events events*))

(def rss-events (events)
  (tag (rss version "2.0")
    (tag channel
      (tag title (pr events-title*))
      (tag link (pr events-url*))
      (tag description (pr events-desc*))
      (map
        (fn (event)
          (tag item
              (tag title (pr (eschtml event!title)))
              (if (no (blank event!url)) (tag link (link event!url)))
              (tag description
                (cdata
                    (pr event!title) (br)
                    (pr (format-date event!date)) (br)
                    (pr (format-address event!address))))))
        events))))
