(require "gtk.arc")

(def get-pixbuf (name)
  "_should_ select an icon depending on name"
  (gdk-pixbuf-new-from-file "test-image.svg" (get-null-pt)))

(def populate (m d)
  (let iter (make-tree-iter)
    (each name (dir d)
      (gtk-list-store-append m iter)
      (gtk-list-store-set-value m iter 0 name)
      (gtk-list-store-set-gval m iter 1 
                               (mkgval (get-pixbuf (string d "/" name))
                                       cptr 'pixbuf)))))

(def run-view ()
  (in-gtk
    (w/win main "File view"
      (withs (model (gtk-list-store-new 'string 'pixbuf)
              iview (gtk-icon-view-new-with-model model))
        (gtk-icon-view-set-text-column iview 0)
        (gtk-icon-view-set-pixbuf-column iview 1)
        (populate model ".")
        (gtk-container-add main iview)))))

(run-view)
