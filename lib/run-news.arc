(load "lib/news.arc")
(load "lib/search.arc") ; search bar
(load "lib/events.arc") ; event calendar
(load "lib/blog.arc")   ; community blogging
(load "lib/boyer-moore.arc") ; faster POST request parsing
(thread (nsv 8080))
(sleep 3)  ; wait for nsv's initial messages to appear before printing first prompt
