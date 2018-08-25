(require '(
 news.arc   ; HN style app
 search.arc ; search bar
 events.arc ; event calendar
 blog.arc   ; community blogging
 lib/prompt.arc
))

(thread (nsv 8080)) ; run in a thread so repl remains usable
(sleep 3)  ; wait for nsv's initial messages to appear before printing first prompt
