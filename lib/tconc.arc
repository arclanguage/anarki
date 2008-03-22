; tconc.arc
; by AmkG
; A fast list concatenation system

(def tconc-new ()
  " Generates a new tconc cell.  A tconc cell
    is a cell which encapsulates a cheap method
    of concatenating lists.  Initialize a variable
    with (tconc-new) and use (tconc var val) to
    concatenate a single value, and use (lconc var
    vals) to concatenate several.
    After concatenation, extract the concatenated
    list using (car var)
    Example:
      (let v (tconc-new)
        (for i 1 1000
          (tconc v i))
        (car v))
    See also [[tconc]] [[lconc]] "
  (cons nil nil))

(def tconc (var val)
  " Concatenates `val' onto the tconc cell stored
    in `var'.
    Create an empty tconc cell by using (tconc-new),
    then concatenate values using (tconc var val)
    and (lconc var vals).
    After concatenation, extract the concatenated
    list using (car var)
    See also [[tconc-new]] [[lconc]] "
  ; we don't use the 'let form because
  ; we're very optimization-paranoid
  ; let: hd = (car var)
  ;      tl = (cdr var)
  (let new (cons val nil)
    ;if hd
    (if (car var)
        (do
          ;= (cdr tl)        new
          (= (cdr (cdr var)) new)
          ;= tl        new
          (= (cdr var) new))
        (do
          ;= hd        new
          (= (car var) new)
          ;= tl        new
          (= (cdr var) new))))
  ; might be unnecessary, but scheme should
  ; optimize this away.
  var)

(let lastcdr (afn (p) (aif (cdr p) (self it) p))
  (def lconc (var vals)
    " Concatenates the list `vals' onto the tconc cell
      stored in `var'.
      Create a new tconc cell using (tconc-new), then
      concatenate values using (lconc var val)
      WARNING!  'lconc will reuse the storage for
      `vals' (i.e. it is destructive).  If you might
      use the list again, copy the list before
      passing the list to 'lconc.
      Example:
        (let v (tconc-new)
          (for i 1 100
            (lconc v (range 1 i)))
          (car v))
      See also [[tconc-new]] [[tconc]] "
    ; let: hd = (car var)
    ;      tl = (cdr var)
    ;if hd
    (if (car var)
    ;then
      ;= (cdr tl)        vals
      (= (cdr (cdr var)) vals)
    ;else
      ;= hd        vals
      (= (car var) vals
      ;= tl        vals
         (cdr var) vals))
    ;zap lastcdr tl
    (zap lastcdr (cdr var))
    var))

