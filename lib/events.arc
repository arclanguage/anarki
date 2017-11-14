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
          (link "rss" "events-rss"))
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
    (o date (today))
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
          (newdate
            (arg req "y")
            (string (pos (arg req "m") months))
            (arg req "d"))
          id)
        (row 'Title   (input 't title 40))
        (row 'Address (input 'a address 40))
        (row 'Link    (input 'u url 40))
        (row 'Date
          (tag span
            (menu  'y (range ((today) 'year)
                                         (+ ((today) 'year) 10))
                                               (int date!year))
            (menu  'm (map [months _] (range 1 12))
                               (months (int date!month)))
            (menu  'd (range 1 31) (int date!day))))
        (row (submit))))))

(def process-event (user title address url date id)
  ; Somehow insufficient check, because February 31st passes
  (if
      (no (all errsafe:int (list date!year date!month date!day)))
        (new-event-page user title address url date id
                       "Please pick a valid date.")
      (past date)
        (new-event-page user title address url date id
                       "Please pick a future date.")
      (blank title)
        (new-event-page user title address url date id
                       "Please enter a title.")
      (nor (valid-url url) (blank url))
        (new-event-page user title address url date id
                       "Please enter a valid link (or no link).")
      (do
        (add-event user title address url date id)
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
    (link
      (ellipsize address)
      (openstreetmap address)) ""))

(def format-date (date)
  (if (sameday (today) date)
      "Today"
      (string (check (int date!year) [isnt _ ((today) 'year)] "") " "
              (months (int date!month)) " "
              date!day)))

(def display-event (user event)
  (row
    (tag span
      (pr (format-title event!title event!url))
      (br)
      (tag span
        (w/bars
          (pr (format-date event!date))
          (pr (format-address event!address))
          (when (may-edit user event)
            (w/bars
              (onlink "edit" (edit-event user event))
              (onlink "delete" (del-confirm-event user event))))))))
  (spacerow 5))

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
                    (pr event!title)
                    (br)
                    (pr (format-date event!date))
                    (br)
                    (pr (format-address event!address))))))
        events))))
