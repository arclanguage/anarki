(require '(
 lib/news.arc   ; HN style app
 lib/search.arc ; search bar
 lib/events.arc ; event calendar
 lib/blog.arc   ; community blogging
))
(thread (nsv 8080)) ; run in a thread so repl remains usable
(sleep 3)  ; wait for nsv's initial messages to appear before printing first prompt
