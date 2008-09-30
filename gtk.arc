;;; Interface towards gtk+ using ffi.arc

;;; This is a direct translation of the gtk API
;;; Tested on Debian GNU/Linux with gtk+-2.6.4, and on Slackware with 
;;; gtk+-2.8.18, with mzscheme 352, 372 and current Arc Anarki version.

(require "ffi.arc")

(w/ffi "gtk/glue"
  (cdef get-tree-iter-size "get_tree_iter_size" culong ())
  (cdef get-gvalue-size "get_gvalue_size" culong ())
  (cdef get-text-iter-size "get_text_iter_size" culong ()))

;; GTypes

(w/ffi "libgdk-x11-2.0"
  (cdef gdk-init "gdk_init" cvoid (cptr cptr))
  (cdef gdk-pixbuf-get-type "gdk_pixbuf_get_type" culong ())
  (cdef gdk-pixbuf-new-from-file "gdk_pixbuf_new_from_file" 
        cptr (cstring cptr)))

(= gtype* (table))
(= gtype->ctype* (table))
(= gtype-list* nil)
(= arc-type-list* nil)
(= type-map* (table))
(def get-g-type (n) (* n 4))
(mac defgtype (name id arc-type ctype)
  "defines a GValue type"
  `(do
     (= (gtype* ,name) (get-g-type ,id))
     (= (gtype->ctype* ,name) ,ctype)
     (push ,name gtype-list*)
     (= (type-map* ,arc-type) ,ctype)
     (push ,arc-type arc-type-list*)))

(defgtype 'char 3 'char cbyte)
(defgtype 'int 6 'int cint)
(defgtype 'string 16 'string cstring)
(defgtype 'pointer 17 'nil cptr)
(defgtype 'pixbuf 0 'nil cptr)
;; object _must_ be added after pixbuf
(defgtype 'object 20 'nil cptr)

;; Handling of GValues, very low level and very unsafe, but it seems to work
(w/inline 
  "void* inc_pt(void *pt, unsigned int offset) 
   {
     return (void*)(((unsigned long)pt)+offset);
   }"
  (cdef inc-pt "inc_pt" cptr (cptr cuint)))

(def mkempty-gval ()
  (let pt (cmalloc (get-gvalue-size))
    (cpset pt culong 0)
    pt))

(def mkgval (val ctype gtype)
  "Builds a GValue from a C type.
   ctype is the C type, gtype is a key in gtype*"
  (let pt (mkempty-gval)
    (cpset pt culong (gtype* gtype))
    (cpset (inc-pt pt (csizeof culong)) ctype val)
    pt))

(def make-gvalue (val)
  "automatically builds a GValue from val"
  (with (ctype nil gtype nil)
    (if (acptr val)
      (do
        (= ctype cptr)
        (each gt gtype-list*
          (if (is (g-type-check-instance-is-a val (gtype* gt)) 1)
            (= gtype (gtype* gt)))))
      (each tp arc-type-list*
        (if (is (type val) tp)
          (= ctype (type-map* (type val)) gtype (gtype* (type val))))))
    (if (no ctype) (err "Invalid type passed to make-gvalue"))
    (let pt (mkempty-gval)
      (cpset pt culong gtype)
      (cpset (inc-pt pt (csizeof culong)) ctype val)
      pt)))

(def get-gvalue (pt)
  "gets contents of a gvalue, automatically discover the type"
  (let ctype cptr ; default
    (each gt gtype-list*
      (if (is (g-type-check-value-holds pt (gtype* gt)) 1) 
        (= ctype (gtype->ctype* gt))))
    (cpref (inc-pt pt (csizeof culong)) ctype)))

;; TreeIter
(def make-tree-iter ()
  (cmalloc (get-tree-iter-size)))

;; TextIter
(def make-text-iter ()
  (cmalloc (get-text-iter-size)))

;; needed to get a NULL C pointer
(w/inline 
  "void* get_null_pt() { return (void*)0; }" 
  (cdef get-null-pt "get_null_pt" cptr ()))

(def gtkname->arc-name (s (o prefix "gtk_"))
  (let str (string prefix s)
    (forlen i str
      (if (is (str i) #\_) (= (str i) #\-)))
    (sym str)))

(def get-n-syms (n)
  (if (is n 0) nil (cons (uniq) (get-n-syms (- n 1)))))

(def need-aux (in-args)
  (some [or (acons _) (is _ 'gvalue)] in-args))

(def build-aux (arc-name aux-name ret-type args)
  ;; auxiliary lists
  (with (aux-args nil real-args nil out-args nil out-args-t nil)
    (each el args
      (if (acons el)
        (let tp (car el)
          (push tp out-args-t)
          (w/uniq name
            (push name out-args)
            (push name aux-args)))
        (w/uniq name
          (push name real-args)
          (push name aux-args))))
     (= aux-args (rev aux-args)) 
     (= real-args (rev real-args))
     (= out-args (rev out-args))
     (= out-args-t (rev out-args-t))
     
     `(def ,arc-name ,real-args
       (with ,(mappend (fn (x y) 
                         (if (is y 'gvalue) `(,x (mkempty-gval))
                             (is y 'tree-iter) `(,x (make-tree-iter))
                             (is y 'text-iter) `(,x (make-text-iter))
                             `(,x (cmalloc (csizeof ,y)))))
                       out-args out-args-t)
         (let res (,aux-name ,@(map (fn (x y)
                                      (if (is x 'gvalue) `(make-gvalue ,y) y))
                                    args aux-args))
           ,(let to-pass (map (fn (x y)
                                (if (is y 'gvalue) `(get-gvalue ,x)
                                    (or (is y 'tree-iter) (is y 'text-iter)) x
                                    `(cpref ,x ,y)))
                              out-args out-args-t)
              (if (is ret-type 'cvoid)
                (if (is (len to-pass) 1) 
                  (car to-pass)
                  `(list ,@to-pass))
                `(list res ,@to-pass))))))))

(mac gtkdef (name . rest)
  "imports a gtk function. Handles automatically gvalues (type gvalue).
   If an argument type is within parenthesis, a pointer to it will be 
   automatically created and its value returned in a list. 
   If the value to return is just one, it is returned directly, without 
   putting it into a list.
   Example: (gtkdef widget_get_pointer cvoid (cptr (cint) (cint)))
   creates a function gtk-widget-get-pointer wich takes a cptr and returns
   a list of two integers"
  (with (cname (string "gtk_" (if (acons name) (cadr name) name))
         arc-name (gtkname->arc-name (if (acons name) (car name) name)))
    (if (need-aux (cadr rest))
      (let aux-name (sym (string arc-name "-aux"))
        `(do
           (cdef ,aux-name ,cname ,(car rest)
                ,(map [if (is _ 'gvalue) 'cptr (acons _) 'cptr _] (cadr rest)))
           ,(build-aux arc-name aux-name (car rest) (cadr rest))))
      `(cdef ,arc-name ,cname ,@rest))))

(mac gdef (name . rest)
  (with (cname (string "g_" (if (acons name) (cadr name) name))
         arc-name (gtkname->arc-name (if (acons name) (car name) name) "g_"))
    (if (need-aux (cadr rest))
      (let aux-name (sym (string arc-name "-aux"))
        `(do
           (cdef ,aux-name ,cname ,(car rest)
                ,(map [if (is _ 'gvalue) 'cptr (acons _) 'cptr _] (cadr rest)))
           ,(build-aux arc-name aux-name (car rest) (cadr rest))))
      `(cdef ,arc-name ,cname ,@rest))))

;; TODO: add possibility to specify default values to arguments

(mac defenum (name . args)
  "defines an enumeration type"
  (let counter -1
    `(do 
       (= ,name (table))
       ,@(map (fn (arg)
                (if (is 'quote (car arg))
                  `(= (,name ,arg) ,(++ counter))
                   (do 
                     (= counter (cadr arg))
                     `(= (,name ,(car arg)) ,counter))))
              args))))

(defenum justification 'left 'right 'center 'fill)
(defenum resize-mode 'parent 'queue 'immediate)
(defenum widget-state 'normal 'active 'prelight 'selected 'insensitive)
(defenum window-type 'toplevel 'popup)
(defenum window-pos 'node 'center 'mouse 'center-always 'center-on-parent)
(defenum policy-type 'always 'automatic 'never)
(defenum menu-direction 'parent 'child 'next 'prev)
(defenum wrap-mode 'node 'char 'word 'word-char)
(defenum pango-style 'normal 'oblique 'italic)
(defenum pango-weight ('ultralight 200) ('light 300) ('normal 400)
                      ('semibold 600) ('bold 700) ('ultrabold 800) 
                      ('heavy 900))

(w/ffi "libgtk-x11-2.0"

  ;; initialization & misc
  (gtkdef (init-aux "init") cvoid (cint cptr))
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
  (gtkdef widget_get_pointer cvoid (cptr (cint) (cint)))
  (gtkdef widget_is_ancestor cint (cptr cptr))
  (gtkdef widget_translate_coordinates cint 
              (cptr cptr cint cint (cint) (cint)))
  (gtkdef widget_hide_on_delete cint (cptr))
  (gtkdef widget_set_style cvoid (cptr cptr))
  (gtkdef widget_ensure_style cvoid (cptr))
  (gtkdef widget_get_style cptr (cptr))
  (gtkdef widget_get_parent cptr (cptr))
  (gtkdef widget_path cvoid (cptr (cint) (cstring) (cstring))) 

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
  (gtkdef image_get_pixmap cvoid (cptr (cptr) (cptr)))
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

  ;; list store
  (gtkdef (list_store_newv_aux "list_store_newv") cptr (cint cvec))
  (gtkdef list_store_set_value cvoid (cptr cptr cint gvalue))
  (gtkdef list_store_insert cvoid (cptr (tree-iter) cint))
  (gtkdef list_store_append cvoid (cptr (tree-iter)))
  (gtkdef list_store_remove cint (cptr cptr))
  (gtkdef list_store_clear cvoid (cptr))

  ;; tree model
  (gtkdef tree_model_get_iter cvoid (cptr (tree-iter) cptr))
  (gtkdef tree_model_get_value cvoid (cptr cptr cint (gvalue))) 
  (gtkdef tree_iter_copy cptr (cptr))

  ;; icon view
  (gtkdef icon_view_new cptr ())
  (gtkdef icon_view_new_with_model cptr (cptr))
  (gtkdef icon_view_set_model cvoid (cptr cptr))
  (gtkdef icon_view_set_text_column cvoid (cptr cint))
  (gtkdef icon_view_set_pixbuf_column cvoid (cptr cint))

  ;; scrolled window
  (gtkdef scrolled_window_new cptr (cptr cptr))
  (gtkdef scrolled_window_set_policy cvoid (cptr cuint cuint))
  (gtkdef scrolled_window_add_with_viewport cvoid (cptr cptr))

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

  ;; check menu item
  (gtkdef check_menu_item_new cptr ())
  (gtkdef check_menu_item_new_with_label cptr (cstring))
  (gtkdef check_menu_item_new_with_mnemonic cptr (cstring))
  (gtkdef check_menu_item_get_active cint (cptr))
  (gtkdef check_menu_item_set_active cvoid (cptr cint))
  (gtkdef check_menu_item_set_show_toggle cvoid (cptr cint))
  (gtkdef check_menu_item_get_inconsistent cint (cptr))
  (gtkdef check_menu_item_set_inconsistent cvoid (cptr cint))
  (gtkdef check_menu_item_set_draw_as_radio cvoid (cptr cint))
  (gtkdef check_menu_item_get_draw_as_radio cint (cptr))

  ;; menu shell
  (gtkdef menu_shell_append cvoid (cptr cptr))
  (gtkdef menu_shell_prepend cvoid (cptr cptr))
  (gtkdef menu_shell_insert cvoid (cptr cptr cint))
  (gtkdef menu_shell_deactivate cvoid (cptr))

  ;; menu bar
  (gtkdef menu_bar_new cptr ())

  ;; menu
  (gtkdef menu_new cptr ())
  (gtkdef menu_popup cvoid (cptr cptr cptr cfptr cptr cuint cuint))
  (gtkdef menu_set_title cvoid (cptr cstring))
  (gtkdef menu_get_tearoff_state cint (cptr))
  (gtkdef menu_get_title cstring (cptr))
  (gtkdef menu_popdown cvoid (cptr))
  (gtkdef menu_set_tearoff_state cvoid (cptr cint))

  ;; entry
  (gtkdef entry_new cptr ())
  (gtkdef entry_new_with_max_length cptr (cint))
  (gtkdef entry_set_text cvoid (cptr cstring))
  (gtkdef entry_append_text cvoid (cptr cstring))
  (gtkdef entry_prepend_text cvoid (cptr cstring))
  (gtkdef entry_set_position cvoid (cptr cint))
  (gtkdef entry_get_text cstring (cptr))
  (gtkdef entry_select_region cvoid (cptr cint cint))
  (gtkdef entry_set_visibility cvoid (cptr cint))
  (gtkdef entry_set_editable cvoid (cptr cint))
  (gtkdef entry_set_max_length cvoid (cptr cint))
  (gtkdef entry_set_alignment cvoid (cptr cfloat))
  (gtkdef entry_get_alignment cfloat (cptr))
  (gtkdef entry_get_max_length cint (cptr))
  (gtkdef entry_get_visibility cint (cptr))

  ;; text view
  (gtkdef text_view_new cptr ())
  (gtkdef text_view_new_with_buffer cptr (cptr))
  (gtkdef text_view_get_buffer cptr (cptr))
  (gtkdef text_view_set_wrap_mode cvoid (cptr cint))
  (gtkdef text_view_get_wrap_mode cint (cptr))
  (gtkdef text_view_set_editable cvoid (cptr cint))
  (gtkdef text_view_get_editable cint (cptr))
  (gtkdef text_view_set_justification cvoid (cptr cint))
  (gtkdef text_view_get_justification cint (cptr))
  (gtkdef text_view_get_iter_at_location cvoid (cptr cptr cint cint))
  (gtkdef text_view_get_line_at_y cvoid (cptr cptr cint (cint)))
  
  ;; text buffer
  (gtkdef text_buffer_new cptr ())
  (gtkdef text_buffer_get_line_count cint (cptr))
  (gtkdef text_buffer_get_char_count cint (cptr))
  (gtkdef text_buffer_insert cvoid (cptr cptr cstring cint))
  (gtkdef text_buffer_insert_at_cursor cvoid (cptr cstring cint))
  (gtkdef text_buffer_delete cvoid (cptr cptr cptr))
  (gtkdef text_buffer_set_text cvoid (cptr cstring cint))
  (gtkdef text_buffer_get_text cstring (cptr cptr cptr))
  (gtkdef text_buffer_insert_pixbuf cvoid (cptr cptr cptr))
  (gtkdef text_buffer_get_iter_at_line cvoid (cptr (text-iter) cint))
  (gtkdef text_buffer_get_iter_at_offset cvoid (cptr (text-iter) cint))
  (gtkdef text_buffer_get_start_iter cvoid (cptr (text-iter)))
  (gtkdef text_buffer_get_end_iter cvoid (cptr (text-iter)))
  (gtkdef text_buffer_apply_tag cvoid (cptr cptr cptr cptr))
  (gtkdef text_buffer_get_tag_table cptr (cptr))
;  (gtkdef text_buffer_create_tag cptr (cptr cstring))

  ;; Text Iter
  (gtkdef text_iter_forward_chars cint (cptr cint))
  (gtkdef text_iter_get_offset cint (cptr))

  ;; TextTag
  (gtkdef text_tag_new cptr (cstring))

  ;; Text Tag Table
  (gtkdef text_tag_table_add cvoid (cptr cptr))

  ;; tree view
  (gtkdef tree_view_new cptr ())
;  (gtkdef tree_view_get_level_indentation cint (cptr))
;  (gtkdef tree_view_get_show_expanders cint (cptr))
;  (gtkdef tree_view_set_level_indentation cvoid (cptr cint))
;  (gtkdef tree_view_set_show_expanders cvoid (cptr cint))
  (gtkdef tree_view_new_with_model cptr (cptr))
  (gtkdef tree_view_get_model cptr (cptr))
  (gtkdef tree_view_set_model cvoid (cptr cptr))

)

(def gtk-init ()
  (gtk-init-aux 0 (get-null-pt))
  (= gtype*!pixbuf (gdk-pixbuf-get-type)))

(def gtk-list-store-new types
  (gtk-list-store-newv-aux (len types) 
                           (l->cvec (map gtype* types) cint)))

;; easily defines a menu
;; use:
;; (gtk-defmenu menu-label 
;;   [(item-label item-name signal-body) | submenu]*)
(mac gtk-defmenu (label . args)
  "defines a gtk menu"
  (w/uniq (main smain)
    `(with (,main (gtk-menu-item-new-with-label ,label)
            ,smain (gtk-menu-new))
       (gtk-menu-item-set-submenu ,main ,smain)
       ,@(map [if (and (acons _) (is (type (car _)) 'string))
                `(let ,(cadr _) (gtk-menu-item-new-with-label ,(car _))
                   (gtk-menu-shell-append ,smain ,(cadr _))
                   (w/sig ,(cadr _) "activate" ,@(cddr _)))
                `(gtk-menu-shell-append ,smain ,_)]
            args)
       ,main)))

;; simple signal handling
(w/ffi "libgobject-2.0"
  (cdef g-type-check-instance-is-a "g_type_check_instance_is_a" 
        cint (cptr culong))
  (cdef g-type-is-a "g_type_is_a" cint (culong culong))
  (cdef g-type-check-value-holds "g_type_check_value_holds"
        cint (cptr culong))
  (cdef gvalue-unset "g_value_unset" cvoid (cptr))
  (cdef gvalue-reset "g_value_reset" cptr (cptr))
  (cdef g-signal-connect "g_signal_connect_object" culong 
        (cptr cstring cfptr cptr cuint))

  ;; GObject
  (gdef object_set_property cvoid (cptr cstring gvalue))
  (gdef object_get_property cvoid (cptr cstring (gvalue)))
)

;; this list will hold all defined callbacks
;; without this, they would be garbage collected, because Scheme's runtime
;; doesn't know if the foreign runtime references or not the callbacks
;; this way, I'm sure that they'll never be garbage collected
(= callbacks* nil)

(mac gtk-def-connect (name mac-name . in-parameters)
  "defines callback connection handling function and macro for callbacks
   with input parameters in-parameters"
  (let par-names (get-n-syms (len in-parameters))
    `(do
       (def ,name (widget signal fun)
         (let cb (ffi-callback fun (list ,@in-parameters) cint)
           (push cb callbacks*)
           (g-signal-connect widget signal cb (get-null-pt) 0)))
       (mac ,mac-name (widget signal ,@par-names . body)
         "execute body when signal is triggered on widget"
         (w/uniq f
           `(do
              (def ,f ,,(cons 'list par-names) ,@body 1)
              (,,name ,widget ,signal ,f)))))))

(gtk-def-connect connect-0 w/sig)
(gtk-def-connect connect-1 w/sig1 cptr)
(gtk-def-connect connect-2 w/sig2 cptr cptr)
(gtk-def-connect connect-3 w/sig3 cptr cptr cptr)

;; models
;; with models to call a method on a widget w one can say:
;; (w!methodname arg1 ...) instead of (gtk-widget-name-methodname w arg1 ...)
;; Ex: (gtk-window-set-title w "title") becomes (w!set-title "title")
;; and (gtk-widget-show-all w) becomes (w!show-all)
;; To achieve this _every_ object is an hash table containing a reference to
;; _every_ function defined for that widget (plus its ancestors)
;; In conclusion, they save a lot of typing and code gets cleaner, but there is
;; a _huge_ memory consumption drawback.
;; TODO: add a guide on how models work

(def copy-table (tb to-tb)
  (maptable (fn (k v) (= (to-tb k) v)) tb)
  to-tb)

(= gtk-models* (table))

(def gtk-mkmodel (name ptr)
  "makes an instance of a model with pointer ptr"
  (let m (table)
    (copy-table (gtk-models* name) m)
    ; captures the table in every function and automatically pass m!ptr
    (maptable (fn (k v) (= (m k) (fn args (apply v m!ptr args)))) m)
    (= m!ptr ptr)
    m))

;; (gtk-def-model mod nil 
;;   build-fn-name
;;   add rem show ...)
(mac gtk-def-model (name parent build-fn-name . fnames)
  (w/uniq m
    `(let ,m (table)
       ,(if parent `(copy-table (gtk-models* ',parent) ,m))
       (= (gtk-models* ',name) ,m)
       ,(if build-fn-name
          `(def ,(sym (string build-fn-name "-m")) args 
             (gtk-mkmodel ',name (apply ,build-fn-name args))))
       ,@(map [let full-name (sym (string name "-" _))
               `(= (,m ',_) ,full-name)]
              fnames))))

(gtk-def-model gtk-widget nil
  nil
  path
  get-pointer
  show-all
  show)

(gtk-def-model gtk-label gtk-widget
  gtk-label-new
  set-text)

(gtk-def-model gtk-button gtk-widget
  gtk-button-new-with-label)

(gtk-def-model gtk-container gtk-widget
  nil 
  add remove)

(gtk-def-model gtk-box gtk-container 
  nil pack-start)

(gtk-def-model vbox gtk-box gtk-vbox-new)
(gtk-def-model hbox gtk-box gtk-hbox-new)

(gtk-def-model gtk-window gtk-container
  gtk-window-new
  set-title
  set-position
  set-default-size)

;; tests

(mac in-gtk body
  `(protect (fn ()
              (gtk-init)
              ,@body
              (gtk-main))
            (fn ()
              (gtk-main-quit))))

(mac defgtkmain (name args . body)
  `(def ,name ,args
     (in-gtk
       ,@body)))

(mac w/win (name title . body)
  `(let ,name (gtk-window-new (window-type 'toplevel))
     (gtk-window-set-title ,name ,title)
     (gtk-window-set-default-size ,name 400 250)
     (gtk-window-set-position ,name (window-pos 'center))
     (w/sig2 ,name "destroy" w user
       (gtk-main-quit))
     ,@body
     (gtk-widget-show-all ,name)))

(mac w/win-m (name title . body)
  `(let ,name (gtk-window-new-m (window-type 'toplevel))
     ((,name 'set-title) ,title)
     ((,name 'set-default-size) 400 250)
     ((,name 'set-position) (window-pos 'center))
     (w/sig2 (,name 'ptr) "destroy" w user
       (gtk-main-quit))
     ,@body
     (gtk-widget-show-all (,name 'ptr))))

(defgtkmain gtk-hello-world ()
  (w/win w "Hello, World!"
    (let btn (gtk-button-new-with-label "Hello, World!")
      (gtk-container-add w btn)
      (w/sig2 btn "clicked" b user
        (gtk-widget-show-all 
          (gtk-message-dialog-new w 0 0 0 "Hello, World!"))))))

;(gtk-hello-world)

(defgtkmain test ()
  (w/win-m w "Test"
    (with (v (gtk-vbox-new-m 0 0)
           l (gtk-label-new-m "")
           btn (gtk-button-new-with-label-m "Print pointer position"))
      (w/sig btn!ptr "clicked"
        (let pos (w!get-pointer)
          (l!set-text (string "(" (car pos) " " (cadr pos) ")")))
        (prn (btn!path)))
      (v!pack-start l!ptr 1 1 0)
      (v!pack-start btn!ptr 0 1 0)
      (w!add v!ptr))))

;(test)

(defgtkmain test-btns ()
  (w/win w "Buttons test"
    (with (hb-bt (gtk-hbox-new 0 0)
           hb-tog (gtk-hbox-new 0 0)
           vb (gtk-vbox-new 0 0)
           btns (map gtk-button-new-with-label:string '(1 2 3 4 5 6))
           toggles (map gtk-toggle-button-new-with-label:string '(1 2 3 4))) 
      (each btn btns
        (w/sig2 btn "clicked" bt data
          (prn (gtk-widget-path bt))
          (gtk-container-remove hb-bt bt))
        (gtk-container-add hb-bt btn))
      (each tog toggles
        (gtk-container-add hb-tog tog))
      (gtk-container-add vb hb-bt)
      (gtk-container-add vb hb-tog)
      (gtk-container-add w vb))))

;(test-btns)

;; there must be an image named "test-image.svg" inside the directory for
;; the test to work 
(defgtkmain test-iconview ()
  (w/win w "IconView test"
    (let model (gtk-list-store-new 'string 'pixbuf)
      (let iview (gtk-icon-view-new-with-model model)
        (w/sig2 iview "item_activated" v path
          (let it (gtk-tree-model-get-iter model path)       
            (prn (gtk-tree-model-get-value model it 0))
            (gtk-list-store-remove model it)))
        (gtk-icon-view-set-text-column iview 0)
        (gtk-icon-view-set-pixbuf-column iview 1)
        (for i 0 10
          (let iter (gtk-list-store-append model)
            (gtk-list-store-set-value model iter 0 "Text under image")
            (let pix (gdk-pixbuf-new-from-file 
                       "test-image.svg" (get-null-pt))
              (gtk-list-store-set-value model iter 1 pix))))
        (gtk-container-add w iview)))))

;(test-iconview)

(def showmsg (parent msg)
  (gtk-widget-show-all
    (gtk-message-dialog-new parent 0 0 0 msg)))

(defgtkmain test-menu ()
  (w/win w "Test menu"
    (withs (vb (gtk-vbox-new 0 0)
            bar (gtk-menu-bar-new))
      (gtk-box-pack-start vb bar 0 1 0)
      (gtk-menu-shell-append 
        bar
        (gtk-defmenu "File"
          ("Quit" quit
            (gtk-main-quit))))
      (gtk-menu-shell-append 
        bar
        (gtk-defmenu "Sub 2"
          ("Item 1" it2-1
            (showmsg w "Item 2-1"))
          ("Item 2" it2-2
            (showmsg w "Item 2-2"))
          (gtk-defmenu "Sub 2.1" 
            ("Item 2.1" it2-1-1
              (showmsg w "Item2.1-1")))))
      (gtk-container-add w vb))))

;(test-menu)

(defgtkmain test-treeview ()
  (w/win w "Test TreeView"
    (withs (model (gtk-list-store-new 'string)
            view (gtk-tree-view-new-with-model model)
            data (map string '(One Two Three Four Five Six)))
      (each s data
        (gtk-list-store-set-value model (gtk-list-store-append model) 0 s))
      (gtk-container-add w view))))

;(test-treeview)
