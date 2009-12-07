($ (require file/md5))

(def md5 (str)
  (($ bytes->string/utf-8) (($ md5) (($ string->bytes/utf-8) str))))
