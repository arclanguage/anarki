;; view a directory and travel through directories

(require "gtk.arc")

(gtk-init)

(with (file-pix (gdk-pixbuf-new-from-file "gtk/file.svg" (get-null-pt))
       dir-pix (gdk-pixbuf-new-from-file "gtk/dir.svg" (get-null-pt)))
  (def get-pixbuf (name)
    "select an icon depending on name"
    (if (dir-exists name) dir-pix file-pix)))

(def populate (m d)
  (let iter (make-tree-iter)
    (each name (cons ".." (dir d))
      (gtk-list-store-append m iter)
      (gtk-list-store-set-value m iter 0 name)
      (gtk-list-store-set-value m iter 1 (get-pixbuf (string d "/" name))))))

(def run-view ()
  (in-gtk
    (w/win main "View directory"
      (withs (d "." ; currently viewed directory
              model (gtk-list-store-new 'string 'pixbuf)
              iview (gtk-icon-view-new-with-model model)
              scroll (gtk-scrolled-window-new (get-null-pt) (get-null-pt)))
        (w/sig2 iview "item_activated" iv path
          (gc)
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
        (gtk-container-add main scroll)))))

(run-view)
