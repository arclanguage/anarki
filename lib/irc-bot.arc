(def client (hostname port)
  ($ (call-with-values 
         (lambda () (tcp-connect ,hostname ,port))
       list)))

(= server* "localhost")

(def log args
  (w/appendfile o "irclog"
     (w/stdout o (apply prs args) (prn))))

(def out args
  (w/stdout op*
    (let args (join args (list "\r"))
      (apply prn args )
      (apply log "=>" args)))
  nil)

(def parse (s)
  (let toks (fn (s) (let ts (tokens s)
                      (cons (map sym (tokens (car ts) #\!))
                            (map sym (cdr ts)))))
    (aif (findsubseq ":" s 1) 
         (join (toks (cut s 0 it))
               (list (cut s (+ it 1))))
         (toks s))))

(def irc (nick)
  (let (ip op)
    (client server* 6667)
    (= op* op)                          ;so we can send stuff from the repl
    (w/stdin ip
      (out "NICK " nick)
      (out
       "USER "          (or (env "USER") "unknown")
       " unknown-host " server*
       " :"             "arcbot"
       ", version "     "0")

      (whilet l (readline)
        (= l (trim (trim l 'end) 'front #\:))
        (log "<=" l)
        (let l (parse l)
          (case (caar l)
            NOTICE (log  "ooh, a notice:" (cdr l))
            PING   (out "PONG :" (cadr l))
            (case (cadr l)
              |001|    (map [out "JOIN " _]  (list "##arcbot"))
              |433|    (do (log "Oh hell, gotta whop the nick.")
                           (close ip)
                           (close op)
                           (irc (+ nick "_")))
              JOIN     (log "user" (car l) "joined" (car:cdr:cdr l))
              PRIVMSG  (withs ((speaker privmsg dest text) l
                               toks (tokens text)
                               url-regexp (re "http(s)?(//[-a-zA-Z0-9_.]+:[0-9]*)?[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;.,]+[-a-zA-Z0-9_=#$@~`%&*+\\/]")
                               grab-matches (fn (re str)
                                                (drain (aif (re-pos re str)
                                                            (let (start . stop) (car it)
                                                                 (do1 (cut str start stop)
                                                                      (= str (cut str stop))))))))
                              (map [out "NOTICE " dest " :" (tinyurl _)]
                                   (keep [< 75 (len _)] (grab-matches url-regexp text))
                                   ))
              (log "?"))))))))

(def irc& (nick)
  (= bot* (thread (irc nick))))
