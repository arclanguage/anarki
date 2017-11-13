;   Event calendar for news.arc
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

(= eventdir* (+ srvdir* "events/")
   event-maxid* 0
   events* nil)

(= event-threshold* 5)

(deftem event  id      nil
               by      nil
               title   nil
               address nil
               link    nil
               date    nil)

(deftem date  year  nil
              month nil
              day   nil)

(def newdate (y m d)
  (inst 'date 'year  y
              'month m
              'day  d))

(= months
  '(nil "Jan" "Feb" "Mar" "Apr" "May" "Jun"
    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(def today ()
  (let now ($ (seconds->date (current-seconds)))
    (newdate ($.date-year now) ($.date-month now) ($.date-day now))))

(def compare-dates (f d1 d2)
  (if (isnt (int d1!year)  (int d2!year))
      (f    (int d1!year)  (int d2!year))
      (isnt (int d1!month) (int d2!month))
      (f    (int d1!month) (int d2!month))
      (f    (int d1!day)   (int d2!day))))

(def earlier (d1 d2)
  (compare-dates < d1 d2))

(def later (d1 d2)
  (compare-dates > d1 d2))

(def sameday (d1 d2)
  (compare-dates is d1 d2))

(def future (d)
  (earlier (today) d))

(def past (d)
  (later (today) d))

(def upcoming (events)
  (rem [past _!date]
    (sort (fn (e1 e2) (earlier e1!date e2!date))
       events)))

(newsop events ()
  (events-page user))

(def events-page (user)
  (ensure-events)
  (longpage user (msec) nil "events" "Events" "events"
    (tab
      (if (organizer user)
        (center (link "new event" "new-event")))
      (center
        (widtable 600
          (each event (upcoming events*)
            (display-event user event)))))))

(def load-events ()
   (each id (map int (dir eventdir*))
      (do
      (= event-maxid* (max event-maxid* id))
    (push (temload 'event (string eventdir* id)) events*)))
   events*)

(def ensure-events ()
  (ensure-dir eventdir*)
  (if (no events*)
      (load-events)))

(newsop new-event ()
  (if
    (no user)
      (login-page
        "You need to be logged in to post events."
        (fn (user ip) (new-event-page user)))
    (< (karma user) event-threshold*)
      (pr "Sorry, you need " event-threshold* " karma to post events.")
    (new-event-page user)))

(def new-event-page
    (user
    (o title)
    (o address)
    (o link)
    (o date (today))
    (o msg))
  (minipage "New event"
    (pagemessage msg)
    (uform user req
      (process-event
        (get-user req)
        (arg req "t")
        (arg req "a")
        (arg req "l")
        (newdate
          (arg req "y")
          (string (pos (arg req "m") months))
          (arg req "d")))
      (tab
        (row 'Title   (input 't title 40))
        (row 'Address (input 'a address 40))
        (row 'Link    (input 'l link 40))
        (row 'Date
          (tag span
            (menu  'y (range ((today) 'year)
                                         (+ ((today) 'year) 10))
                                               (int date!year))
            (menu  'm (map [months _] (range 1 12))
                               (months (int date!month)))
            (menu  'd (range 1 31) (int date!day))))
        (row (submit))))))

(def process-event (user title address link date)
  ; Somehow insufficient check, because February 31st passes
  (if (no (all errsafe:int (list date!year date!month date!day)))
        (new-event-page user title address link date
                       "Please pick a valid date.")
      (past date)
        (new-event-page user title address link date
                       "Please pick a future date.")
      (blank title)
        (new-event-page user title address link date
                       "Please enter a title.")
      (nor (valid-url link) (blank link))
        (new-event-page user title address link date
                       "Please enter a valid link (or no link).")
      (do
        (add-event user title address link date)
        (events-page user))))

(def add-event (user title address link date)
  (ensure-events)
  (let event   (inst 'event
      'id      (++ event-maxid*)
      'by      user
      'title   title
      'address address
      'link    link
      'date    date)
    (save-event event)
    (push event events*)))

(def save-event (event) (temstore 'event event (string eventdir* event!id)))

(def openstreetmap (address)
  (string "https://nominatim.openstreetmap.org/search?q="
          (urlencode address)))

(def display-event (user event)
  (row
    (tag span
      ([if (no (blank event!link))
         (link _ event!link)
         (pr _)] (ellipsize event!title 80))
      (br)
      (spanclass subtext
        (w/bars
          (pr (format-date event!date))
          (if (nonblank event!address)
            (link
              (ellipsize event!address 80)
              (openstreetmap event!address)))
          (if (organizer user) (onlink "delete" (del-confirm-event user event))))))
    (spacerow 5)))

(def format-date (date)
  (if (sameday (today) date)
      (tag b (pr "Today"))
      (string (check (int date!year) [isnt _ ((today) 'year)] "") " "
              (months (int date!month)) " "
              date!day)))

(def organizer (user)
  (and user
       (or (> (karma user) event-threshold*)
           (admin user))))

(def del-confirm-event (user event)
  (minipage "Confirm"
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
  (if (organizer user)
      (do
        (pull [is event _] events*)
        (aif (file-exists (string eventdir* event!id)) (rmfile it))
        (events-page user))
      (pr "Not allowed.")))
