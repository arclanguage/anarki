(def client (hostname port)
  ($ (call-with-values 
         (lambda () (tcp-connect ,hostname ,port))
       list)))

;; be nice to make this settable, so we could do (= (env "FOO") "bar")
(= env ($ getenv))

(= server* "localhost")
(def >err (msg . args)
  (w/stdout (stderr)
    (apply prn msg args)))

(def out args
  (let args (join args (list "\r"))
    (apply prn args )
    (disp "=>" (stderr))
    (map [write _ (stderr)] args)
    (disp "\n" (stderr)))
  nil)

(= chans* (table))

(def parse (s)
  (let toks (fn (s) (let ts (tokens s)
                      (cons (map sym (tokens (car ts) #\!))
                            (map sym (cdr ts)))))
    (aif (findsubseq ":" s 1) 
         (join (toks (subseq s 0 it))
               (list (subseq s (+ it 1))))
         (toks s))))

(def irc (nick)
  (let (ip op)
    (client server* 6667)
    (w/stdin ip
      (w/stdout op
        (out "NICK " nick)
        (out
         "USER "          (or (env "USER") "unknown")
         " unknown-host " server*
         " :"             "arcbot"
         ", version "     "0")

        (whilet l (readline)
          (= l (trim (trim l 'end) 'front #\:))
          (>err "<=" l)
          (let l (parse l)
            (case (caar l)
              NOTICE (>err  "ooh, a notice:" (cdr l))
              PING   (out "PONG :" (cadr l))
              (case (cadr l)
                |001|    (map [out "JOIN " _]  (list "#fart" "#poop"))
                |433|    (do (>err "Oh hell, gotta whop the nick.")
                             (irc (+ nick "_")))
                JOIN     (>err "user" (car l) "joined" (car:cdr:cdr l))
                PRIVMSG  (withs ((speaker privmsg dest text) l
                                 toks (tokens text))
                                (if (headmatch nick (car toks))
                                    ;; TODO: beware of botwars.
                                    (out "PRIVMSG " dest " :" (car speaker) ", you like me!" )
                                    (out "PRIVMSG " dest " :yeah, whatever")))
                (>err "?")))))))))
