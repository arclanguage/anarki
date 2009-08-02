(doclist
  tokens
  " Splits `s' into tokens using `sep' (a character or predicate) as separator.
    Splits on contiguous runs of `sep's - no empty tokens are returned:

        arc> (tokens \"foo  bar\" whitec)
        (\"foo\" \"bar\")

    See also [[slices]] [[positions]] [[halve]] "

  slices
  " Splits `s' into slices using `sep' (a character or predicate) as separator. 
    Splits on each occurrence of `sep' - a run of `sep's produces empty slices:

        arc> (slices \"foo  bar\" whitec)
        (\"foo\" \"\" \"bar\")

    See also [[tokens]] [[positions]] [[halve]] "

  halve
  " Splits `s' on the first instance of `sep' (a character or predicate).

    In more detail: Returns a list whose car is `s' up to the first occurrence
    of `sep', or all of `s' if `sep' never appears, and whose cadr (if `sep'
    appears) is the rest of `s' (including the leading `sep').

    See also [[tokens]] [[cut]] [[pos]] "

  positions
  " Lists all indices in `seq' which pass `test'. "

  lines
  " Splits `s' into a list of lines (not including line-terminators).
    See also [[tokens]] [[positions]] "

  urldecode
  " Decodes the string `s' as per application/x-www-form-urlencoded.
    See also [[urlencode]] "

  urlencode
  " Encodes the string `s' as per application/x-www-form-urlencoded.
    See also [[urldecode]] "

  litmatch
  " Test if `seq' starting at offset `start' begins with `pat'. Because of the
    macro expansion, `pat' must be a literal.
    See also [[endmatch]] [[headmatch]] [[begins]] "

  endmatch
  " Test if `seq' ends with `pat'. Because of the macro expansion, `pat' must be
    a literal.
    See also [[litmatch]] "

  posmatch
  " Return the index (from `start') where `pat' appears in `seq'. If `pat' is a
    predicate, it is applied to the characters of `seq' until it returns true.
    See also [[findsubseq]] [[pos]] "

  headmatch
  " Tests if `seq' from offset `start' onwards starts with `pat'. `headmatch'
    will die if `pat' is longer than `seq' and matches up to the end of `seq'.
    See also [[begins]] [[litmatch]] "

  begins
  " Tests if `seq' begins with `pat'. Equivalent to `(headmatch pat seq start)',
    except it doesn't die if matching goes past the end of `seq'.
    See also [[headmatch]] [[litmatch]] "

  subst
  " Substitutes `new' for `old' in `seq'. `new' can be any printable object.
    See also [[multisubst]] "

  multisubst
  " For each pair in `pairs', substitutes its cadr for its car in `seq'.
    See also [[subst]] "

  findsubseq
  " Finds the index where `pat' appears in `seq', starting at `start'.
    As `posmatch', but doesn't accept a function for `pat'.
    See also [[posmatch]] "

  blank
  " Tests if `str' is blank (entirely whitespace).
    See also [[nonblank]] [[whitec]] "

  nonblank
  " Returns nil if `s' is blank, and `s' otherwise.
    See also [[blank]] "

  trim
  " Trims character which pass `test' from `str'. `where' can be 'front, to trim
    the front of the string, 'back, to trim the end; or 'both, to trim both. "

  num
  " Formats a real number. `digits' is the number of digits after the decimal
    point, `trail-zeroes' indicates whether trailing zeros should be included,
    and `init-zero' indicates whether there should be a zero before the
    decimal point (if `(< -1 n 1)'). "

  pluralize
  " Returns `str' pluralized. If `n' is 1 or a list of length 1, `str' is
    returned unchanged; otherwise an `s' is appended.
    See also [[plural]] "

  plural
  " Returns a string \"<n> <x>\" representing `n' of `x' in english, pluralizing
    `x' if necessary.
    See also [[pluralize]] "
  )
