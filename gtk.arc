;;; Interface towards gtk+ using ffi.arc
;;; This is a direct translation of the gtk API
;;; Tested on Debian GNU/Linux with gtk+-2.6.4, should also work with other
;;; versions.

(require "ffi.arc")

;; needed to get a NULL C pointer
(w/inline 
  "void* get_null_pt() { return 0; }" 
  (cdef get-null-pt "get_null_pt" cptr ()))

(def gtkname->arc-name (s)
  (let str (string "gtk_" s)
    (forlen i str
      (if (is (str i) #\_) (= (str i) #\-)))
    (sym str)))

(mac gtkdef (name . rest)
  "imports a gtk function"
  (with (cname (string "gtk_" (if (acons name) (cadr name) name))
         arc-name (gtkname->arc-name (if (acons name) (car name) name)))
    `(cdef ,arc-name ,cname ,@rest)))

(mac defenum (name . args)
  "defines an enumeration type"
  (let counter -1
    `(do 
       (= ,name (table))
       ,@(map (fn (arg) `(= (,name ,arg) ,(++ counter))) args))))

(defenum justification 'left 'right 'center 'fill)
(defenum resize-mode 'parent 'queue 'immediate)
(defenum widget-state 'normal 'active 'prelight 'selected 'insensitive)
(defenum window-type 'toplevel 'popup)
(defenum window-pos 'node 'center 'mouse 'center-always 'center-on-parent)

(w/ffi "libgtk-x11-2.0.so"

  ;; initialization & misc
  (gtkdef init cvoid (cint cptr))
  (gtkdef main cvoid ())
  (gtkdef main_quit cvoid ())

  ;; widget
  (gtkdef widget_destroy cvoid (cptr))
  (gtkdef widget_unparent cvoid (cptr))
  (gtkdef widget_map cvoid (cptr))
  (gtkdef widget_unmap cvoid (cptr))
  (gtkdef widget_realize cvoid (cptr))
  (gtkdef widget_unrealize cvoid (cptr))
  (gtkdef widget_show cvoid (cptr))
  (gtkdef widget_hide cvoid (cptr))
  (gtkdef widget_show_all cvoid (cptr))
  (gtkdef widget_hide_all cvoid (cptr))
  (gtkdef widget_add_accelerator cvoid (cptr cstring cptr cuint cuint cuint))
  (gtkdef widget_remove_accelerator cint (cptr cptr cuint cuint))
  (gtkdef widget_activate cint (cptr))
  (gtkdef widget_reparent cvoid (cptr cptr))
  (gtkdef widget_is_focus cint (cptr))
  (gtkdef widget_grab_focus cvoid (cptr))
  (gtkdef widget_grab_default cvoid (cptr))
  (gtkdef widget_set_state cvoid (cptr cuint))  
  (gtkdef widget_set_sensitive cvoid (cptr cint))
  (gtkdef widget_set_parent cvoid (cptr cptr))
  (gtkdef widget_set_parent_window cvoid (cptr cptr))
  (gtkdef widget_get_parent_window cptr (cptr))
  (gtkdef widget_set_uposition cvoid (cptr cint cint))
  (gtkdef widget_set_usize cvoid (cptr cint cint))
  (gtkdef widget_set_events cvoid (cptr cint))
  (gtkdef widget_add_events cvoid (cptr cint))
  (gtkdef widget_get_events cint (cptr))
  (gtkdef (widget_get_pointer_aux "widget_get_pointer") cvoid (cptr cptr cptr))
  (gtkdef widget_is_ancestor cint (cptr cptr))
  (gtkdef widget_translate_coordinates cint (cptr cptr cint cint cptr cptr))
  (gtkdef widget_hide_on_delete cint (cptr))
  (gtkdef widget_set_style cvoid (cptr cptr))
  (gtkdef widget_ensure_style cvoid (cptr))
  (gtkdef widget_get_style cptr (cptr))
  (gtkdef widget_get_parent cptr (cptr))
  (gtkdef (widget_path_aux "widget_path") cvoid (cptr cptr cptr cptr))  

  ;; label
  (gtkdef label_new cptr (cstring))
  (gtkdef label_set_text cvoid (cptr cstring))
  (gtkdef label_set_attributes cvoid (cptr cptr))
  (gtkdef label_set_markup cvoid (cptr cstring))
  (gtkdef label_set_markup_with_mnemonic cvoid (cptr cstring))
  (gtkdef label_set_pattern cvoid (cptr cstring))
  (gtkdef label_set_justify cvoid (cptr cint)) 
  (gtkdef label_parse_uline cuint (cptr cstring))
  (gtkdef label_set_line_wrap cvoid (cptr cint))
  (gtkdef label_get_text cstring (cptr))
  (gtkdef label_get_label cstring (cptr))
  (gtkdef label_set_label cvoid (cptr cstring))

  ;; image
  (gtkdef image_get_pixbuf cptr (cptr))
  (gtkdef (image_get_pixmap_aux "image_get_pixmap") cvoid (cptr cptr cptr))
  (gtkdef image_new_from_file cptr (cstring))
  (gtkdef image_set_from_file cvoid (cptr cstring))
  (gtkdef image_new cptr ())
  
  ;; container
  (gtkdef container_add cvoid (cptr cptr))
  (gtkdef container_remove cvoid (cptr cptr))
  (gtkdef container_get_resize_mode cint (cptr))
  (gtkdef container_set_resize_mode cvoid (cptr cint))
  (gtkdef container_set_border_width cvoid (cptr cint))
  (gtkdef container_get_border_width cint (cptr))

  ;; bin
  (gtkdef bin_get_child cptr (cptr))

  ;; button
  (gtkdef button_new cptr ())
  (gtkdef button_new_with_label cptr (cstring))
  (gtkdef button_set_label cvoid (cptr cstring))
  (gtkdef button_get_label cstring (cptr))

  ;; toggle button
  (gtkdef toggle_button_new cptr ())
  (gtkdef toggle_button_new_with_label cptr (cstring))
  (gtkdef toggle_button_new_with_mnemonic cptr (cstring)) 
  (gtkdef toggle_button_get_active cint (cptr))
  (gtkdef toggle_button_set_active cvoid (cptr cint))
  (gtkdef toggle_button_get_inconsistent cint (cptr))
  (gtkdef toggle_button_set_inconsistent cvoid (cptr cint))  

  ;; check button
  (gtkdef check_button_new cptr ())
  (gtkdef check_button_new_with_label cptr (cstring))
  (gtkdef check_button_new_with_mnemonic cptr (cstring))

  ;; radio button
  (gtkdef radio_button_new cptr (cptr))
  (gtkdef radio_button_new_from_widget cptr (cptr))
  (gtkdef radio_button_new_with_label cptr (cptr cstring))
  (gtkdef radio_button_new_with_label_from_widget cptr (cptr cstring))
  (gtkdef radio_button_set_group cvoid (cptr cptr))
  (gtkdef radio_button_get_group cptr (cptr))

  ;; option menu
  (gtkdef option_menu_new cptr ())
  (gtkdef option_menu_get_menu cptr (cptr))
  (gtkdef option_menu_set_menu cvoid (cptr cptr))
  (gtkdef option_menu_remove_menu cvoid (cptr))
  (gtkdef option_menu_set_history cvoid (cptr cuint))
  (gtkdef option_menu_get_history cuint (cptr))

  ;; item
  (gtkdef item_select cvoid (cptr))
  (gtkdef item_deselect cvoid (cptr))
  (gtkdef item_toggle cvoid (cptr))

  ;; menu item
  (gtkdef menu_item_new cptr ())
  (gtkdef menu_item_new_with_label cptr (cstring))
  (gtkdef menu_item_set_submenu cvoid (cptr cptr))
  (gtkdef menu_item_remove_submenu cvoid (cptr))
  (gtkdef menu_item_get_submenu cptr (cptr))
 
  ;; window
  (gtkdef window_new cptr (cint))
  (gtkdef window_set_title cvoid (cptr cstring))
  (gtkdef window_set_default_size cvoid (cptr cint cint))
  (gtkdef window_set_resizable cvoid (cptr cint))
  (gtkdef window_set_position cvoid (cptr cint))
  (gtkdef window_resize cvoid (cptr cint cint))

  ;; message dialog
  (gtkdef message_dialog_new cptr (cptr cint cint cint cstring))

  ;; box
  (gtkdef box_pack_start cvoid (cptr cptr cint cint cint))
  (gtkdef box_pack_end cvoid (cptr cptr cint cint cint))
  
  ;; vbox
  (gtkdef vbox_new cptr (cint cint))

  ;; hbox
  (gtkdef hbox_new cptr (cint cint))
)

(mac last-pts (f types . args)
  "calls function with arguments args and extra parameters in the end 
   as pointers to the types in types, and returns the contents of the
   memory referenced by the pointers in a list"
  (w/uniq res
    (let pts (map [list (uniq) _] types)
      `(with ,(join
               (mappend
                 (fn (x) `(,(car x) (cmalloc (csizeof ,(cadr x)))))
                  pts)
               `(,res nil))
         (,f ,@args ,@(map car pts))
         (= ,res (list ,@(map (fn (x) `(cpref ,(car x) ,(cadr x))) pts)))
         ,res))))

(def gtk-widget-path (w)
  (last-pts gtk-widget-path-aux (cint cstring cstring) w)) ; seg. faults
(def gtk-widget-get-pointer (w) 
  (last-pts gtk-widget-get-pointer-aux (cint cint) w))
(def gtk-translate-coordinates (w1 w2 x y) 
  (last-pts gtk-widget-translate-coordinates (cint cint) w1 w2 x y))
(def gtk-image-get-pixmap (img) 
  (last-pts gtk-image-get-pixmap-aux (cptr cptr) img))

;; simple signal handling
(w/ffi "libgobject-2.0"
  (cdef g-signal-connect "g_signal_connect_data" culong 
        (cptr cstring (cfn (list) cint) cint cint cint)))

(def connect (widget signal fun)
  (g-signal-connect widget signal (fn () (fun) 1) 0 0 0))

(mac w/sig (widget signal . body)
  "execute body when signal is triggered on widget"
  `(connect ,widget ,signal (fn () ,@body)))

;; tests

(mac in-gtk body
  `(do 
     (gtk-init 0 (get-null-pt))
     ,@body
     (gtk-main)))

(mac w/win (name title . body)
  `(let ,name (gtk-window-new (window-type 'toplevel))
     (gtk-window-set-title ,name ,title)
     (gtk-window-set-default-size ,name 300 200)
     (gtk-window-set-position ,name (window-pos 'center))
     (w/sig ,name "destroy"
       (gtk-main-quit))
     ,@body))

(def gtk-hello-world ()
  (in-gtk
    (w/win w "Hello, World!"
      (let btn (gtk-button-new-with-label "Hello, World!")
        (gtk-container-add w btn)
        (w/sig btn "clicked"
          (gtk-widget-show-all 
            (gtk-message-dialog-new w 0 0 0 "Hello, World!")))
      (gtk-widget-show-all w)))))

;(gtk-hello-world)

(def test ()
  (in-gtk
    (w/win w "Test"
      (with (v (gtk-vbox-new 0 0)
             l (gtk-label-new "")
             btn (gtk-button-new-with-label "Print pointer position"))
        (w/sig btn "clicked"
          (let pos (gtk-widget-get-pointer w)
            (gtk-label-set-text l (string "(" (car pos) " " (cadr pos) ")")))
          (prn (gtk-widget-path btn))) ; sometimes segfaults
        (gtk-box-pack-start v l 1 1 0)
        (gtk-box-pack-start v btn 0 1 0)
        (gtk-container-add w v)
        (gtk-widget-show-all w)))))

;(test)

(def test-btns ()
  (in-gtk
    (w/win w "Buttons test"
      (with (hb-bt (gtk-hbox-new 0 0)
             hb-tog (gtk-hbox-new 0 0)
             vb (gtk-vbox-new 0 0)
             btns (map gtk-button-new-with-label:string '(1 2 3 4 5 6))
             toggles (map gtk-toggle-button-new-with-label:string '(1 2 3 4))) 
        (each btn btns
          (gtk-container-add hb-bt btn))
        (each tog toggles
          (gtk-container-add hb-tog tog))
        (gtk-container-add vb hb-bt)
        (gtk-container-add vb hb-tog)
        (gtk-container-add w vb)
        (gtk-widget-show-all w)))))

;(test-btns)
