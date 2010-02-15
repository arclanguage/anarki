($ (require file/md5))
(load "lib/lang.arc")

(def md5 (str)
  (($ bytes->string/utf-8) (($ md5) (($ string->bytes/utf-8) str))))

(def sha224 (data)
  (perl subprocess “
	use Digest::SHA qw(sha224_hex);
	sha224_hex(«data»);
	”))
