;;; Interface towards gtk+ using ffi
;;; This is a direct translation of gtk API
;;; Tested on Debian GNU/Linux with gtk+-2.6.4, should also work with others
;;; versions.

(require "ffi.arc")

(def gtkname->arc-name (s)
  (let str (string "gtk_" s)
    (forlen i str
      (if (is (str i) #\_) (= (str i) #\-)))
    (sym str)))

(mac gtkdef (name . rest)
  "imports a gtk function"
  `(cdef ,(gtkname->arc-name name) ,(string "gtk_" name)  ,@rest))

(mac defenum (name . args)
  "defines an enumeration type"
  (let counter -1
    `(do 
       (= ,name (table))
       ,@(map (fn (arg) `(= (,name ,arg) ,(++ counter))) args))))

(defenum window-type 'toplevel 'popup)
(defenum window-pos 'node 'center 'mouse 'center-always 'center-on-parent)

(w/ffi "libgtk-x11-2.0.so"

  ;; initialization & misc
  (gtkdef init cvoid (cint cint))
  (gtkdef main cvoid ())
  (gtkdef main_quit cvoid ())

  ;; widget
  (gtkdef widget_show_all cvoid (cptr))

  ;; container
  (gtkdef container_add cvoid (cptr cptr))

  ;; button
  (gtkdef button_new cptr ())
  (gtkdef button_new_with_label cptr (cstring))

  ;; window
  (gtkdef window_new cptr (cint))
  (gtkdef window_set_title cvoid (cptr cstring))
  (gtkdef window_set_default_size cvoid (cptr cint cint))
  (gtkdef window_set_resizable cvoid (cptr cint))
  (gtkdef window_set_position cvoid (cptr cint))
  (gtkdef window_resize cvoid (cptr cint cint))
)

;; simple signal handling
(w/ffi "libgobject-2.0"
  (cdef g-signal-connect "g_signal_connect_data" culong 
        (cptr cstring (cfn (list) cint) cint cint cint)))

(def connect (widget signal fun)
  (g-signal-connect widget signal (fn () (fun) 1) 0 0 0))

(mac w/sig (widget signal . body)
  "execute body when signal is triggered on widget"
  `(connect ,widget ,signal (fn () ,@body)))

(def gtk-hello-world ()
  (gtk-init 0 0)
  (with (w (gtk-window-new (window-type 'toplevel))
         btn (gtk-button-new-with-label "Hello, World!"))
    (gtk-window-set-title w "Hello, World!")
    (gtk-window-set-default-size w 300 200)
    (gtk-window-set-position w (window-pos 'center))
    (w/sig w "destroy"
      (gtk-main-quit))
    (gtk-container-add w btn)
    (w/sig btn "clicked"
      (prn "Hello, World!"))
    (gtk-widget-show-all w)
    (gtk-main)))

;(gtk-hello-world)
