
(require "lib/defpat.arc")
(mac pm (expr)
	" Shortcut for [[pat-match]] "
	`(pat-match ,expr))

