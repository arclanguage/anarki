; Arkani configuration module
; by AmkG
; Configures Arkani-based wikis

#|The intent is that in the future, instead of
explicitly declaring (*wiki-def ...) in your
wiki-arc.conf file, that file will be edited
by this defop.
|#
; The wiki configuration module
(let (help* sig source-file*) nil
  ; protect against arc-wiki def bashing the
  ; global docstrings tables
  (= help* (table) sig (table) source-file* (table))
  (defop wikiconf args
    (w/html
      ('head
        ('title (prn "Wiki-Arc Configuration")))
      ('body
        ('.content
          ('h1 (pr "Wiki-Arc Configuration")))))))


