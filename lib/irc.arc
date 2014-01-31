(def connect (host port) 
  ($ (let-values (((in out) (tcp-connect ,host ,port))) 
		 (list in out))))
(def disconnect (out)
  ($ (close-output-port ,out)))

(def init-irc ()
  (= irc-in* nil
     irc-out* nil
     irc-user* nil
     irc-nick* nil
     irc-server* nil
     irc-logfile* nil
     irc-thread* nil))

(def irc-connect (server user (o password nil))
  (let (i o) (connect server 6667)
       (= irc-in* i
	  irc-out* o
	  irc-server* server)
       (irc-login user password)))

(def irc commands
  (w/stdout irc-out*
    (apply pr commands)
    (pr "\r\n"))
  (pr ">>>")
  (apply pr commands)
  (prn))

(def irc-msg (channel msg)
  (if (> len.msg 440)
      (let (str rest)
           (apply (afn (str . rest)
                       (if (< (len (+ str " " car.rest)) 440) 
                     (apply self (cons (+ str " " car.rest) cdr.rest))
                     (list str (reduce + (intersperse " " rest)))))
            tokens.msg)
           (irc "PRIVMSG " channel " :" str)
           (irc-msg channel rest))
      (irc "PRIVMSG " channel " :" msg)))

(def nick (nick)
  (irc "NICK " nick))

(def ident (user realname)
  (irc "USER " user " 0 * :" realname))

(def part channels
  (irc "PART :" (apply string (intersperse "," channels))))

(def irc-login (user password)
  nick.user
  (ident user user)
  (aif password
       (irc-msg "NickServ" (string "identify " it))))

(def irc-pong (line)
  (if (headmatch "PING" line)
      (irc (subst "PONG" "PING" line))))

(def irc-loop (parse)
  (whilet line (readline irc-in*)
      prn.line
      irc-pong.line
      (aif parse it.line)))

(def irc-parse (line)
  (if (posmatch "PRIVMSG" line)
      (let (nick user host command chan . msg) (tokens line [pos _ ":@! "])
	   (= msg (apply string (intersperse ":" (cdr:tokens line #\:))))
	   (list nick user host command chan msg))
      nil))

(mac w/irc (chan . body)
  `(map [irc-msg ,chan _]
	(fromstring (tostring ,@body) 
	  (drain:readline))))