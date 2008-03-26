;; view a directory and travel through directories

(require "gtk.arc")

(gtk-init)

(with (file-pix (gdk-pixbuf-new-from-file "gtk/file.svg" (get-null-pt))
       dir-pix (gdk-pixbuf-new-from-file "gtk/dir.svg" (get-null-pt)))
  (def get-pixbuf (name)
    "select an icon depending on name"
    (if (dir-exists name) dir-pix file-pix)))

(def populate (m d)
  (each name (cons ".." (dir d))
    (let it (gtk-list-store-append m)
      (gtk-list-store-set-value m it 0 name)
      (gtk-list-store-set-value m it 1 (get-pixbuf (string d "/" name))))))

(defgtkmain run-view ()
  (w/win main "View directory"
    (withs (vb (gtk-vbox-new 0 0)
            menubar (gtk-menu-bar-new)
            d "." ; currently viewed directory
            model (gtk-list-store-new 'string 'pixbuf)
            iview (gtk-icon-view-new-with-model model)
            scroll (gtk-scrolled-window-new (get-null-pt) (get-null-pt)))
      ;; menu
      (gtk-menu-shell-append 
        menubar
        (gtk-defmenu "File"
          ("Quit" quit (gtk-main-quit))))
      (gtk-menu-shell-append
        menubar
        (gtk-defmenu "Help"
          ("About" about 
            (showmsg main "Arc GTK example application\n\nView directory contents"))))
      (gtk-box-pack-start vb menubar 0 1 0)
      (w/sig2 iview "item_activated" iv path
        (withs (it (gtk-tree-model-get-iter model path)
                newdir (string d "/" (gtk-tree-model-get-value model it 0)))
          (when (dir-exists newdir)
            (gtk-list-store-clear model)
            (= d newdir)
            (populate model d))))
      (gtk-icon-view-set-text-column iview 0)
      (gtk-icon-view-set-pixbuf-column iview 1)
      (populate model ".")
      (gtk-scrolled-window-set-policy 
        scroll policy-type!automatic policy-type!automatic)
      (gtk-container-add scroll iview)
      (gtk-container-add vb scroll)
      (gtk-container-add main vb))))

(run-view)
