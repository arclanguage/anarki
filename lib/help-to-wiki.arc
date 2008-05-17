(def disp-all (out-stream . rest)
  (each x rest
    (disp x out-stream)))

(def help-to-wiki (out-name)
  (w/outfile o out-name
    (disp "|_. Name |_. Description |\n" o)
    (maptable (fn (k v) (disp-all o "| " k " | " (helpstr k) " |\n")) help*)
    'done))
