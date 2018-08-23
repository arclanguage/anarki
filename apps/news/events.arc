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

(= eventdir*    (+ srvdir* "events/")
   event-maxid* 0
   events*      nil
   osm-enabled  t)

(def event (id)
  (car (keep [is _!id id] events*)))

(deftem event  id      nil
               by      nil
               title   nil
               address nil
               url     nil
               date    nil
               oclock  nil)

(= months
  '(nil "Jan" "Feb" "Mar" "Apr" "May" "Jun"
        "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(def weekday (d)
  ('("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
  ($.date-week-day ($.seconds->date (toseconds d) #f))))

(def till (d)
  (let days (roundup (- (- (days-since (toseconds d)) 0.5)))
    (if (< days 7)
            (if (is days 0) "today"
                (is days 1) "tomorrow"
                (+ "in " (plural days "day")))
        (< days 30)
            (+ "in " (plural (round (/ days 7)) "week"))
        (< days 365)
            (+ "in " (plural (round (/ days 30)) "month"))
        t
            (+ "in " (plural (round (/ days 365)) "year")))))

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

(newsop events ()
  (events-page user))

(= osmjs* "
var map = {

  init : loc => {
    document.getElementById('map').style.height='200'
    map.context = L.map('map').setView([loc.lat,loc.lon],13)
    L.tileLayer(
      'http://{s}.tile.osm.org/{z}/{x}/{y}.png',
      { attribution:
        '&copy; <a href=http://osm.org/copyright>OpenStreetMap</a> contributors'
      }).addTo(map.context)
  },

  markers: {},

  lookup : event => {
    var address = event.getAttribute('data-address')
    if (address) fetch(
      'https://nominatim.openstreetmap.org/search?q='+
      address+
      '&format=json'
    ).then(
      res => res.json()
    ).then(
      locs => {
        if (locs.length > 0) {
          if (!map.context) {
            map.init(locs[0])
          }
          ((marker, event) => {
            var label =
              '<a href=viewevent?id='+
              event.getAttribute('data-id')+'>'+
              event.getAttribute('data-title')+'</a><br>'
            if (! marker ) {
              map.markers[locs[0].osm_id] = L.marker(
                [locs[0].lat,
                locs[0].lon]
              ).addTo(map.context).bindPopup(
                label, {maxHeight: 100}
              )
              marker =  map.markers[locs[0].osm_id]
            } else {
              marker.bindPopup(marker.getPopup().getContent()+label)
            }
            event.onclick = function(){
              map.context.flyTo([locs[0].lat,locs[0].lon])
            }
          })(map.markers[locs[0].osm_id], event)
        }
      }
    )
  }
}

window.onload = _ => {
  for ( var e of document.getElementsByClassName('event') ) {
    map.lookup(e)
  }
} ")

(def osm ()
  (when osm-enabled
    (tag (div "id" "map" "width" "100%" "height" "0"))
    (tag (link "rel" "stylesheet" "type" "text/css"
               "href" "https://unpkg.com/leaflet@1.2.0/dist/leaflet.css"))
    (tag (script "src" "https://unpkg.com/leaflet@1.2.0/dist/leaflet.js"))
    (tag "script" (pr osmjs*))))

(def events-page (user)
  (ensure-events)
  (longpage user (msec) nil "events" "Events" "events"
    (tab
      (osm)
      (center
          (w/bars
            (link "new event" "newevent")
            (link "rss" "events-rss")))
      (each event (upcoming events*)
        (display-event user event)))))

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

(newsop newevent ()
  (if
    (no user)
      (login-page
        "You need to be logged in to post events."
        (fn (user ip) (new-event-page user)))
    (new-event-page user)))

(def edit-event (user event)
  (if (may-edit user event)
    (new-event-page
      user
      event!title
      event!address
      event!url
      event!date
      event!oclock
      event!id)
      "Not allowed."))

(def new-event-page
    (user
    (o title)
    (o address)
    (o url)
    (o d (date))
    (o oclock)
    (o id)
    (o msg))
  (minipage "New event"
    (pagemessage msg)
    (tab
      (urform user req
        (process-event
          (get-user req)
          (arg req "t")
          (arg req "a")
          (arg req "u")
          (list
            (arg req "year")
            (string (pos (arg req "month") months))
            (arg req "day"))
          (arg req "oclock")
          id)
        (row "title"   (input 't title 40))
        (row "address" (input 'a address 40))
        (row "link"    (input 'u url 40))
        (spacerow 10)
        (row "year"  (menu  'year  (range ((date) 0) (+ ((date) 0) 10)) (int (d 0))))
        (row "month" (menu  'month (map [months _] (range 1 12)) (months (int (d 1)))))
        (row "day"   (menu  'day   (range 1 31) (int (d 2))))
        (row "time of day" (input 'oclock oclock 10))
        (spacerow 10)
        (row (submit))))))

(def process-event (user title address url d oclock id)
  (let error [new-event-page user title address url d oclock id _]
    (if
      (no (errsafe:toseconds d))
        (flink [error "Please pick a valid date."])
      (past d)
        (flink [error "Please pick a future date."])
      (blank title)
        (flink [error "Please enter a title."])
      (> (len title) 80)
        (flink [error "Please make title < 80 characters."])
      (nor (valid-url url) (blank url))
        (flink [error "Please enter a valid link (or no link)."])
      (> (len oclock) 20)
        (flink [error "Please make time of day < 20 characters."])
      (do
        (add-event user title address url d oclock id)
        "events"))))

(def add-event (user title address url date oclock (o id))
  (ensure-events)
  (let event   (inst 'event
      'id      (if (no id) (++ event-maxid*) id)
      'by      user
      'title   (capitalize title)
      'address address
      'url     url
      'date    date
      'oclock  oclock)
    (save-event event)
    (if id (pull [is _!id id] events*))
    (push event events*)))

(def save-event (event) (temstore 'event event (string eventdir* event!id)))

(def format-address (address)
  (tostring
    (if (nonblank address)
        (link (ellipsize address) "#map"))))

(def format-date (d)
  (string (weekday d) " "
          (d 2) " "
          (months (int (d 1))) " "
          (check (int (d 0)) [isnt _ ((date) 0)] "")))

(def format-time (o)
  (check o nonblank ""))

(def display-event (user e)
  (row
    (pr (capitalize (till e!date)))
    (tag
      (span "class"        "event"
            "data-address" e!address
            "data-title"   e!title
            "data-id"      e!id)
      (link e!title
            (check e!url nonblank (event-permalink e)))
      (br)
      (spanclass "subtext"
        (w/bars
          (pr (format-time e!oclock))
          (pr (format-date e!date))
          (pr (format-address e!address))
          (when (may-edit user e)
            (w/bars
              (onlink "edit" (edit-event user e))
              (onlink "delete" (del-confirm-event user e)))))))
    (spacerow 5)))

(def may-edit (user event)
  (and user
       (or (is event!by user)
           (admin user))))

(newsop viewevent (id)
  (ensure-events)
  (let e (event (int id))
    (longpage user (msec) nil nil event!title (event-permalink e)
      (tab
        (osm)
        (display-event user e)))))

(def event-permalink (e)
  (string "viewevent?id=" e!id))

(def del-confirm-event (user event)
  (minipage "Confirm"
    (tab
     (display-event user event)
     (spacerow 20)
     (tr (td)
         (td (urform user req
               (if (is (arg req "b") "Yes")
                 (delete-event user event)
               ;TODO: should really just go back to previous page
                 "events")
               (pr "Do you want this to be deleted?")
               (br2)
               (but "Yes" "b") (sp) (but "No")))))))

(def delete-event (user event)
  (when (may-edit user event)
        (pull [is event _] events*)
        (aif (file-exists (string eventdir* event!id)) (rmfile it))
        "events"))

(defop events-rss ()
  (ensure-events)
  (rss-events events*))

(def rss-events (events)
  (tag (rss version "2.0")
    (tag channel
      (tag title (pr (+ this-site* " events")))
      (tag link (pr (+ site-url* "events")))
      (tag description (pr site-desc*))
      (map
        (fn (e)
          (tag item
            (tag title (pr (eschtml e!title)))
            (tag link  (pr (if (blank e!url)
                               (event-permalink e)
                               (eschtml e!url))))
            (if (no (blank e!url)) (tag link (link e!url)))
            (tag description
              (cdata
                (pr e!title) (br)
                (pr (format-time e!oclock)) (br)
                (pr (format-date e!date)) (br)
                (pr e!address)))))
        events))))
