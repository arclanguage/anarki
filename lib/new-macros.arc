; new-macros.arc -- born on 2008/02/20
;
; Quick and dirty script to find all the macros that are loaded by
; default in Anarki. Binds three symbols:
;
; `known-arc1-macros' a list of macros in Arc1.
; `known-arc2-macros' a list of macros in Arc2.
; `new-macros' a function that lists all new macros, by analysing
;  source code in the current directory.
;
; It does this largely by string manipulation, as opposed to nice,
; lispy code crawling. Feel free to improve this file, but make sure it
; will still run on vanilla Arc2.

(= known-arc1-macros
   '(++ -- = aand accum afn afnid aform aformh after aif and arform
        arformh assert atlet atomic attribute atwith atwiths awhen case
        caselet catch center check complement compose conswhen defmemo defop
        defop-raw defopl defopr defopr-raw defset deftem do1 drain each
        endmatch errsafe flatlen for forlen form fromstring gentag iflet in
	inputs insort insortnew jtime let linkf litmatch loop matchform
	matchrform max2 n-of new-hspace noisy-each nor obj on onlink ontable
	opmeth or point pop prbold prf prrow pull push pushnew rand-choice
	repeat rfn rlinkf rotate row spacetable spanclass spanrow summing swap
	tab tag tag-if td tdcolor tdright testlit testop textarea thread time
	time10 timed-aform tostring tr trtd underline unless until userlink
	w/appendfile w/bars w/infile w/instring w/link w/link-if w/outfile
	w/outstring w/rlink w/socket w/stdin w/stdout w/table w/uniq when
	when-usermatch when-usermatchr whenlet while whiler whilet whitepage
	widtable wipe with withs zap zerotable))

(= known-arc2-macros 
   '(++ -- = aand accum addtem adop afn afnid aform aformh after aif and
        arform arformh assert atlet atomic attribute atwith atwiths awhen case
        caselet catch cdata center check complement compose conswhen defhook
        defmemo defop defop-raw defopa defope defopg defopl defopr defopr-raw
        defopt defset deftem do1 drain each endmatch errsafe flatlen for forlen
        form fromstring fulltop gentag iflet in inputs insort insortnew jtime
        karma let linkf litmatch longpage loop max2 minipage n-of new-hspace
        newscache newsop noisy-each nor npage obj on onlink ontable opexpand
        opmeth or point pop prbold prf prrow pull push pushnew rand-choice
        repeat rfn rlinkf rotate row shortpage spanclass spanrow sptab summing
        swap tab tag tag-if td tdcolor tdright testlit testop textarea thread
        time time10 timed-aform tostring tr trav trtd uform underline unless
        until urform userlink uvar w/appendfile w/bars w/infile w/instring
        w/link w/link-if w/outfile w/outstring w/rlink w/socket w/stdin w/stdout
        w/table w/uniq when when-umatch when-umatch/r whenlet while whiler
        whilet whitepage widtable wipe with withs zap zerotable))

(withs (get-mac
        (fn (s)
	 "Given line `s', grab name of first macro definition in it,
if it is isn't quoted. This logic is tad weak, but it works for
reasonable input."
	 (if (> (len s) 4)
	     (awhen (posmatch "(mac " s)
	       (let prefix (s (max 0 (- it 1)))
                 (when (no (find prefix "'`"))
		   (cut s (+ it 4)))))))

        get-macs-from-stream
        (afn (str)
          "Return list of macros defined in stream `str'."
          (awhen (readline str)
            (aif (get-mac it) (cons (read it) (self str))
                 (self str))))

        get-macs-from-file
        (fn (file)
          "Return list of macros defined in `file'."
          (w/infile in file
            (get-macs-from-stream in)))

        get-sorted-macs-from-files
        (fn (files)
          "Return sorted list of macros defined in `files'."
          (sort < (apply join (map get-macs-from-file files))))

        get-libs
        (fn (file)
          "Grab list from `file', which presumably looks like
`libs.arc'. Makes foolhardy assumptions about the structure of the
file."
          (w/infile in file
            (awhen (read in)
              ((it 2) 1))))

        scan-files
        (fn ()
          "Get names of macros in arc.arc and associated libs."
          (get-sorted-macs-from-files (cons "arc.arc" (get-libs "libs.arc")))))

  (def new-macros ((o known known-arc2-macros))
    "Scans `arc.arc' and the files loaded in `libs.arc' for macro
definitions. Returns names of all macros not in `known'."
    (rem [find _ known] (scan-files))))