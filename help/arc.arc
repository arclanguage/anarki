(mac doclist args
  `(= ,@(mappend (fn ((name doc)) `((help* ',name) ,doc))
                 (pair args))))

; TODO: defs union addtem since {minutes,hours,days}-since defcache only
; positive len< len> or= hook defhook out fromdisk diskvar disktable todisk

(doclist
  do
  " Evaluates each expression in sequence and returns the result of the
    last expression.
    See also [[do1]] [[after]] "

  def
  " Defines a function with the given `name', `parms', and `body'.
    See also [[fn]] [[mac]] "

  caar " Equivalent to (car (car xs)) "
  cadr " Equivalent to (car (cdr xs)) "
  cddr " Equivalent to (cdr (cdr xs)) "
  no " Returns `t' iff `x' is `nil'. "

  acons
  " Determines if `x' is a `cons' cell or list.
    Unlike 'alist, this function will return nil if given an empty list
    See also [[atom]] [[alist]] [[dotted]] [[isa]] [[cons]] [[list]] "

  atom
  " Determines if `x' is atomic.
    See also [[acons]] [[isa]] "

  copylist
  " Makes a shallow copy of the list `xs'.
    See also [[list]] [[cons]] "

  list
  " Creates a list from the given parameters.
    See also [[cons]] [[acons]] [[copylist]] "

  idfn " Returns `x'; the identity function. "

  map1
  " Return a sequence with function f applied to every element in sequence xs.
    See also [[map]] [[each]] [[mappend]] [[andmap]] [[ormap]] "

  pair
  " Applies pairs of elements to the function `f'.
    See also [[tuples]] [[map]] "

  mac
  " Defines a macro, a special function which transforms code.
    See also [[def]] "

  and
  " Evaluates arguments till false is found else returns the last one.
    See also [[or]] [[aand]] [[andf]] [[nor]] "

  assoc
  " Finds a (key value) pair in an associated list.
    See also [[alref]] [[listtab]] [[tablist]] "

  alref
  " Get a value from a key in a associated list.
    See also [[assoc]] [[listtab]] [[tablist]] "

  with
  " Assigns a set of local variables for the given `body'.
    Assignment is simultaneous.
    See also [[withs]] [[let]] [[fn]] [[do]] "

  let
  " Assigns a local variable for the given `body'.
    See also [[with]] [[withs]] [[fn]] [[do]] "

  withs
  " Assigns local variables for the given `body'.
    The assignments are made in the given order.
    See also [[with]] [[let]] [[fn]] [[do]] "

  join
  " Joins all list arguments together.
    See also [[cons]] [[+]] "

  rfn
  " Creates a function which calls itself as `name'.
    See also [[fn]] [[afn]] "

  afn
  " Creates a function which calls itself with the name `self'.
    See also [[fn]] [[rfn]] [[aif]] [[awhen]] [[aand]] "

  compose
  " Function composition; eg. ((compose x y z) a b c) == (x (y (z a b c)))
    Composes in functional position are transformed away by ac.
    Arc expands x:y:z into (compose x y z)
    See also [[complement]] "

  complement
  " Function complement; eg. ((complement f) a b) == (no (f a b))
    Complements in functional position are transformed away by ac.
    Arc expands ~x into (complement x)
    See also [[compose]] "

  rev
  " Reverses a copy of the list `xs'
    See also [[copy]] [[copylist]] "

  isnt
  " Inverse of is.
    See also [[no]] [[is]] "

  w/uniq
  " Assigns a set of variables to unique symbols.
    Generally used in macros.
    See also [[uniq]] "

  or
  " Computes arguments until one of them is true and returns that result.
    See also [[and]] [[orf]] [[nor]] [[check]] "

  alist
  " Return true if argument is a possibly empty list
    Unlike 'acons, this function returns t when given an empty list
    See also [[atom]] [[acons]] [[dotted]] [[isa]] [[cons]] [[list]] "

  in
  " Returns true if the first argument is one of the other arguments.
    See also [[some]] [[mem]] "

  iso
  " Isomorphic compare - compares structure (can be slow).
    See also [[is]] "

  when
  " When `test' is true, do `body'.
    See also [[unless]] [[if]] [[awhen]] "

  unless
  " When `test' is not true, do `body'.
    See also [[when]] [[if]] [[no]] "

  while
  " While `test' is true, perform `body' in a loop.
    See also [[until]] [[loop]] [[whilet]] [[whiler]] [[for]]
    [[repeat]] [[drain]] "

  empty
  " Test to see if `seq' is an empty list or other sequence.
    See also [[no]] [[acons]] [[len]] "

  reclist
  " Applies the function `f' on the sublists of `xs' until `f' returns true.
    See also [[map]] "

  recstring
  " Applies the function `test' on indices of `s' until `test' returns true.
    See also [[map]] [[reclist]] "

  testify
  " Turns `x' into a test. Functions are returned unchanged. Otherwise, returns
    a function that tests whether its argument is `x'.
    See also [[is]] "

  some
  " Determines if at least one element of `seq' satisfies `test'.
    See also [[all]] [[mem]] [[in]] [[pos]] [[testify]] "

  all
  " Determines if all elements of `seq' satisfy `test'.
    See also [[some]] [[testify]] "

  mem
  " Returns the sublist of `seq' whose first element satisfies `test'.
    See also [[find]] [[some]] [[in]] [[testify]] "

  find
  " Returns the first element of `seq' that satisfies `test'.
    See also [[mem]] [[some]] [[in]] [[testify]] "

  isa
  " Checks if x is of type y.
    See also [[acons]] [[alist]] [[atom]] "

  map
  " Applies the elements of the sequences to the given function.
    Returns a sequence containing the results of the function.
    See also [[each]] [[map1]] [[mappend]] [[reduce]] "

  mappend
  " Applies the elements of the sequences to the given function.
    Returns a sequence containing the concatenation of the results
    of the function.
    See also [[map]] [[join]] "

  firstn
  " Returns the first `n' elements of the given list `xs'.
    See also [[cut]] [[nthcdr]] [[retrieve]] "

  nthcdr
  " Returns the sublist of `xs' starting on the `n'th element.
    `n' is 0-based.
    See also [[cut]] [[firstn]] "

  tuples
  " Returns a list of lists of the elements of `xs', grouped by `n'.
    See also [[pair]] "

  caris
  " Determines if (car x) is `val'.
    See also [[is]] [[car]] [[carif]] "

  warn
  " Displays a warning message on its arguments.
    See also [[ero]] [[pr]] "

  atomic
  " Performs `body' atomically, blocking other threads.
    See also [[atlet]] [[atwith]] [[atwiths]] "

  atlet
  " Performs a `let' atomically, blocking other threads.
    See also [[atomic]] [[atwith]] [[atwiths]] "

  atwith
  " Performs a `with' atomically, blocking other threads.
    See also [[atomic]] [[atlet]] [[atwiths]] "

  atwiths
  " Performs a `withs' atomically, blocking other threads.
    See also [[atomic]] [[atlet]] [[atwith]] "

  defset
  " Defines a setter for the named form.
    See also [[=]] "

  =
  " Assigns values to variables.
    See also [[set]] [[wipe]] [[++]] [[--]] [[rotate]] [[defset]] "

  loop
  " First performs `start'; while `test' is true, performs `body' then
    `update' in a loop.
    See also [[while]] "

  for
  " Loops for the variable `v' from `init' to `max'.
    See also [[repeat]] [[forlen]] "

  repeat
  " Repeats the `body' `n' times.
    See also [[for]] [[forlen]] [[n-of]] "

  walk
  " Calls `func' on each element of `seq' for side-effects.
    See also [[map]] [[each]] "

  each
  " Performs `body' for each element of the sequence returned by `expr',
    with each element assigned to `var'.
    See also [[walk]] [[forlen]] [[on]] [[map]] [[ontable]] "

  cut
  " Returns a subsequence of the given `seq'. If `end' is negative,
    it's a 0-based index from the end of the string. For example,

      arc> (cut \"abcde\" 1, -1)
      \"bcd\"

    See also [[firstn]] [[nthcdr]] [[split]] "

  whilet
  " While `test' is true, perform `body' in a loop.
    The result of `test' is assigned to `var'.
    See also [[while]] [[whiler]] [[drain]] "

  last " Returns the last element of `seq'. "

  rem
  " Returns a copy of `seq' with elements that pass `test' removed.
    See also [[keep]] [[pull]] "

  keep
  " Returns a copy of `seq' with elements that fail `test' removed.
    See also [[rem]] [[pull]] [[trues]] "

  trues
  " Returns (map f xs) sans nils.
    See also [[rem]] [[keep]] "

  do1
  " Performs the body in sequence, then returns the value of the
    first expression.
    See also [[do]] "

  caselet
  " Matches the result of `expr' to arguments until one matches.
    The result of `expr' is assigned to `var'.
    See also [[case]] [[if]] [[iflet]] "

  case
  " Matches the result of `expr' to arguments until one matches.
    See also [[caselet]] [[if]] "

  push
  " Pushes the value `x' on the front of the list in `place'.
    See also [[pop]] [[cons]] "

  swap
  " Swaps the values of the specified places.
    See also [[rotate]] [[=]] "

  rotate
  " Rotates the values of the specified places, from right to left.
    See also [[swap]] [[=]] "

  pop
  " Pops a value from the front of the list in `place'.
    See also [[push]] [[car]] "

  adjoin
  " Returns a list with `x' in front of `xs', unless `test' returns true
    for some element of `xs' when matched with `x'.
    See also [[cons]] [[pushnew]] [[consif]] "

  pushnew
  " Pushes `x' into the front of the list in `place' unless it is
    already in that list.
    See also [[push]] [[adjoin]] [[cons]] [[consif]] "

  pull
  " Removes all elements that pass `test' from the list in `place'.
    See also [[rem]] [[keep]] "

  ++
  " Increments `place' by the given increment `i' (defaults to 1).
    See also [[--]] [[zap]] [[=]] "

  --
  " Decrements `place' by the given decrement `i' (defaults to 1).
    See also [[++]] [[zap]] [[=]] "

  zap
  " Modifies `place' with the result of `op' on that `place'.
    See also [[++]] [[--]] [[pull]] [[push]] [[pop]] [[=]]
    [[set]] [[wipe]] "

  pr
  " Prints the arguments.
    See also [[prn]] [[warn]] [[ero]] "

  prn
  " Prints the arguments followed by a newline.
    See also [[pr]] [[warn]] [[ero]] "

  wipe
  " Sets each of the given places to nil.
    See also [[set]] [[zap]] [[=]] "

  set
  " Sets each of the given places to t.
    See also [[wipe]] [[zap]] [[=]] "

  iflet
  " Checks if `expr' is true, and if so, assigns it to `var' and
    performs the `then' clause.
    See also [[caselet]] [[whenlet]] [[if]] "

  whenlet
  " Checks if `expr' is true, and if so, assigns it to `var' and
    performs the `body'.
    See also [[caselet]] [[iflet]] [[when]] [[if]] "

  aif
  " Similar to `if' but assigns the result of 'expr' to the variable `it'.
    See also [[if]] [[awhen]] [[aand]] [[afn]] "

  awhen
  " Similar to `when' but assigns the result of 'expr' to the variable `it'.
    See also [[when]] [[aif]] [[aand]] [[afn]] "

  aand
  " Similar to `and' but assigns the previous expression to the variable `it'.
    See also [[and]] [[aif]] [[awhen]] [[afn]] "

  accum
  " Collects or accumulates the values given to all calls to `accfn' within
    `body' and returns a list of those values.  Order is preserved.
    See also [[summing]] "

  drain
  " Repeatedly evaluates `expr' until it returns nil, then returns a list
    of the true values.
    See also [[while]] [[whiler]] [[whilet]] "

  whiler
  " Performs `body' while `expr' is not `endval', assigning the result of
    `expr' to `var'.
    See also [[while]] [[whilet]] [[drain]] "

  consif
  " Adds `x' to the front of the list `y' if `x' is true.
    See also [[cons]] [[if]] [[adjoin]] [[conswhen]] "

  string
  " Creates a string from its arguments
    See also [[sym]] "

  flat
  " Flattens a nested list.
    See also [[list]] "

  check
  " Returns `x' if it passes `test', otherwise returns `alt'.
    See also [[or]] "

  pos
  " Returns the position of the first element in `seq' that passes `test'.
    See also [[some]] "

  even
  " Determines if a number is even. See also [[odd]] "

  odd
  " Determines if a number is odd. See also [[even]] "

  after
  " Ensures that the body is performed after the expression `x',
    even if it fails.
    See also [[do]]"

  w/infile
  " Opens the given file `name' for input, assigning the stream to `var'.
    The stream is automatically closed on exit from the `body'.
    See also [[w/outfile]] [[w/instring]] [[w/stdin]] [[w/socket]] "

  w/outfile
  " Opens the given file `name' for output, assigning the stream to `var'.
    The stream is automatically closed on exit from the `body'.
    See also [[w/infile]] [[w/appendfile]] [[w/outstring]] [[w/stdout]] "

  w/instring
  " Opens the given string `str' for input, assigning the stream to `var'.
    The stream is automatically closed on exit from the `body'.
    See also [[w/outstring]] [[fromstring]] [[w/infile]] [[w/stdin]]
    [[w/socket]] "

  w/socket
  " Opens the port for listening, assigning the stream to `var'.
    The stream is automatically closed on exit from the `body'.
    See also [[w/infile]] [[w/instring]] [[w/stdin]] "

  w/outstring
  " Opens a string for output, assigning the stream to `var'.
    The stream is automatically closed on exit from the `body'.
    The contents of the string can be accessed via (inside `var')
    See also [[w/instring]] [[tostring]] [[w/outfile]] [[w/stdout]] "

  w/appendfile
  " Opens a file `name' for append, assigning the stream to `var'.
    The stream is automatically closed on exit from the `body'.
    See also [[w/outfile]] [[w/infile]] "

  w/stdout
  " Opens the stream `str' for output; normal printed output from `body'
    is redirected to the stream.
    See also [[w/stdin]] [[w/outfile]] [[w/outstring]] "

  w/stdin
  " Opens the stream `str' for input; normal read input from `body'
    is redirected from the stream.
    See also [[w/stdout]] [[w/infile]] [[w/instring]] [[w/socket]] "

  tostring
  " Returns the printed standard output from `body' as a string.
    See also [[fromstring]] [[w/stdout]] [[w/outstring]] "

  fromstring
  " Redirects read standard input to `body' from the given string `str'.
    See also [[tostring]] [[w/stdin]] [[w/instring]] "

  readstring1
  " Reads a single expression from the string.
    See also [[read]] "

  read
  " Reads a single expression from a string or stream.
    See also [[readstring1]] [[readfile]] [[readfile1]] [[readall]] "

  readfile
  " Reads the expressions from the file `name', and returns a list of
    expressions read from the file.
    See also [[read]] "

  readfile1
  " Reads a single expression from the file `name'.
    See also [[read]] "

  readall
  " Reads the expressions from the string or stream `src', and returns a
    list of expressions read from the file.
    See also [[read]] "

  writefile
  " Writes the value to the file `name'.
    See also [[writefileraw]] "

  sym
  " Returns the symbol for `x'.
    See also [[string]] "

  int " Interprets `x' as a base-`b' integer. "

  rand-choice
  " Returns the result of one of the given `exprs', chosen at random.
    See also [[random-elt]] "

  n-of
  " Repeats `expr' `n' times, then returns the results in a list.
    See also [[repeat]] "

  rand-string
  " Generates a random string of letters and numbers. "

  forlen
  " Loops across the length of the sequence `s'.
    See also [[repeat]] [[each]] [[on]] "

  on
  " Loops across the sequence `s', assigning each element to `var',
    and providing the current index in `index'.
    See also [[each]] [[forlen]] "

  best
  " Selects the best element of `seq' according to `f'.
    `f' is a comparison function between elements of `seq'.
    See also [[max]] [[most]] "

  max
  " Returns the highest argument.
    See also [[min]] [[best]] [[most]] "

  min
  " Returns the lowest argument.
    See also [[max]] [[best]] [[most]] "

  most
  " Selects the element of `seq' with the highest [f _].
    `f' is a score function for elements of `seq'.
    See also [[best]] [[least]] "

  insert-sorted
  " Inserts `elt' into a sequence `seq' sorted by `test'.
    See also [[sort]] [[insort]] [[reinsert-sorted]] "

  insort
  " Inserts `elt' into a sequence in the place `seq' sorted by `test'.
    See also [[insert-sorted]] [[sort]] "

  reinsert-sorted
  " Inserts `elt' into a sequence `seq', partially sorted by `test'.
    See also [[insert-sorted]] [[insortnew]] [[sort]] "

  insortnew
  " Inserts `elt' into a sequence in the place `seq', partially sorted
    by `test'.
    See also [[reinsert-sorted]] [[sort]] "

  memo
  " Creates a function that will store results of calls to the given
    source function.
    For each set of arguments, the source function will only be called
    once; if the memo'ed function is called again with the same arguments,
    it will return the stored result instead of calling the source function.
    See also [[defmemo]] "

  defmemo
  " Defines a function that automatically stores the results of calls.
    For each set of arguments, this function will only execute once.
    If the function is called again with the same arguments, it will
    immediately return the stored result for that set of arguments.
    See also [[memo]] "

  <=
  " Determines if each argument is less than or equal to succeeding
    arguments. "

  >=
  " Determines if each argument is greater than or equal to succeeding
    arguments. "

  whitec
  " Determines if the given `c' is a whitespace character.
    See also [[alphadig]] [[nonwhite]] [[punc]] "

  nonwhite
  " Determines if the given `c' is not a whitespace character.
    See also [[whitec]] [[alphadig]] [[punc]] "

  letter
  " True iff the given character `c' is a letter.
    See also [[alphadig]] [[digit]] "

  digit
  " True iff the given character `c' is a digit.
    See also [[alphadig]] [[letter]] "

  alphadig
  " Determines if the given `c' is an alphanumeric character.
    See also [[letter]] [[digit]] [[whitec]] [[nonwhite]] [[punc]] "

  punc
  " Determines if the given `c' is punctuation character.
    See also [[whitec]] [[nonwhite]] [[alphadig]] [[punc]] "

  readline
  " Reads a string terminated by a newline from the stream `str'. "

  summing
  " Counts the number of times `sumfn' is called with a true value
    within `body'.
    See also [[accum]] "

  sum
  " Sums (map f xs).
    See also [[map]] [[reduce]] [[summing]] "

  treewise
  " Folds across `tree' as a binary tree, calling `base' on atoms (leafs) and
    calling `f' on the results of recursion across the car and the cdr of conses
    (internal nodes).
    See also [[trav]] [[tree-subst]] [[ontree]] "

  carif
  " Returns the first element of a list if the argument is a list.
    See also [[car]] [[caris]] "

  prall
  " Prints several arguments with an initial header and separated by a
    given separator.
    See also [[prs]] "

  prs
  " Prints several arguments separated by spaces.
    See also [[prall]] "

  tree-subst
  " Replaces an element of a list with that list treated as a binary tree.
    See also [[treewise]] [[trav]] "

  ontree
  " Applies `f' (for side effects) to each node of the binary tree `tree'.
    A binary tree is eiher an atom or a cons of two binary trees.
    See also [[treewise]] [[trav]] "

  dotted
  " Determines if `x' is a dotted cons pair.
    See also [[acons]] [[alist]] "

  fill-table
  " Fills `table' with key-value pairs in the `data' list.
    See also [[table]] "

  keys
  " Returns a list of keys in the table or object `h'.
    See also [[vals]] [[table]] "

  vals
  " Returns a list of values in the table or object `h'.
    See also [[keys]] [[table]] "

  tablist
  " Transforms a table or object `h' into an association list.
    See also [[listtab]] [[alref]] [[assoc]] "

  listtab
  " Transforms an association list into a table or object.
    See also [[tablist]] [[alref]] [[assoc]] "

  obj
  " Creates an object with the specified entries.
    See also [[inst]] [[table]] "

  load-table
  " Loads an association list from `file' into a table or object.
    See also [[load-tables]] [[read-table]] [[save-table]] [[listtab]] "

  read-table
  " Loads an association list from the stream `i' into a table or object.
    See also [[load-tables]] [[load-table]] [[write-table]] [[listtab]] "

  load-tables
  " Loads several association lists from `file' into a list of tables or
    objects.
    See also [[load-table]] [[read-table]] "

  save-table
  " Writes a table or object `h' to `file'.
    See also [[write-table]] [[load-table]] [[tablist]] "

  write-table
  " Writes a table or object `h' to the stream `o'.
    See also [[save-table]] [[read-table]] [[tablist]] "

  copy
  " Creates a copy of an existing argument `x'.
    See also [[rev]] "

  abs " Returns the absolute value of a number. "

  round
  " Rounds off a fractional value to the nearest whole number.
    See also [[roundup]] [[to-nearest]] "

  roundup
  " Rounds off a fractional value to the nearest absolute highest
    whole number.
    See also [[round]] [[to-nearest]] "

  nearest
  " Rounds off `n' to the nearest multiple of `quantum'.
    See also [[round]] [[roundup]] "

  avg
  " Averages all numbers in `ns'.  See also [[med]] "

  med
  " Computes the median of `ns' according to the comparison `test'.
    See also [[avg]] "

  sort " Sorts `seq' according to `test'.  See also [[mergesort]] "

  mergesort
  " Sorts a list `lst' according to `less?'.  See also [[merge]] [[sort]] "

  merge " Merges two sorted lists by `less?'.  See also [[mergesort]] "

  bestn
  " Returns a list of the best `n' elements of seq according to
    the comparison function `f'.
    See also [[best]] "

  split
  " Splits `seq' at offset `pos', returning a two-element list of the
    split.
    See also [[cut]] "

  time
  " Prints the time consumed by evaluating `expr', returning the result.
    See also [[jtime]] [[time10]] "

  jtime
  " Prints the time consumed by `expr', returning `ok' when the
    expression completes.
    See also [[time]] [[time10]] "

  time10
  " Prints the time consumed by executing `expr' 10 times.
    See also [[time]] [[jtime]] "

  deftem
  " Defines an object template for field values, with inclusion for
    existing templates.
    See also [[inst]] [[templatize]] [[temread]] [[temload]] [[temloadall]] "

  inst
  " Creates an object instantiating a given template.
    See also [[deftem]] [[templatize]] [[temread]] [[temload]] [[temloadall]] "

  temread
  " Reads an association list from the stream `str' and creates an
    object instantiating the given template containing the data in
    the association list.
    See also [[deftem]] [[inst]] [[templatize]] [[temload]] [[temloadall]] "

  templatize
  " Creates an object instantiating a given template containing the
    data in the association list `raw'.
    See also [[deftem]] [[inst]] [[temread]] [[temload]] [[temloadall]] "

  temload
  " Reads an association list from `file' and creates an object
    instantiating the given template containing the data in the
    association list.
    See also [[deftem]] [[inst]] [[templatize]] [[temread]] [[temloadall]] "

  temloadall
  " Reads all association lists from `file' and creates a list
    of objects instantiating the given template containing the
    data in each association list.
    See also [[deftem]] [[inst]] [[templatize]] [[temread]] [[temload]]"

  number
  " Determines if `n' is a number. "

  ; TODO: better description for 'cache
  cache
  " Caches the result of a call to `valf' until a number of seconds
    greater than the result of a call to `timef' have passed. "

  errsafe
  " Executes `expr' and blocks any errors "

  saferead
  " Reads an expression, blocking any errors. "

  safe-load-table
  " Loads a table from `filename', blocking any errors. "

  ensure-dir
  " Ensures that the specified directory exists, and creates it if not
    yet created. "

  date " Returns the date as a triple of numbers: (year month day). "

  datestring " Returns the date as a string in YYYY-MM-DD format."

  count " Counts the number of elements in `x' which pass `test'. "

  ellipsize
  " Trims a string `str' with `...' if it is longer than the given `limit'. "

  rand-elt
  " Returns an element of `seq' chosen by random.  See also [[rand-choice]] "

  until
  " While `test' is false, perform `body' in a loop.
    See also [[while]] "

  before " Determines if `x' exists before `y' in `seq'. "

  orf
  " Creates a function which returns true on its argument if any of the
    given `fns' return true on that argument.
    See also [[andf]] "

  andf
  " Creates a function which returns true on its argument if all of the
    given `fns' return true on that argument.
    See also [[orf]] "

  atend
  " Determines if the index `i' is at or beyond the end of the sequence `s'. "

  multiple " Determines if `x' is a multiple of `y'. "

  nor
  " Computes arguments until one of them returns true, then returns nil,
    or else returns true.
    See also [[and]] [[or]] "

  compare
  " Creates a function that compares using `comparer' the result of `scorer'
    on its arguments. "

  conswhen
  " Adds `x' to the front of `y' if `x' passes the test `f'.
    See also [[consif]] [[adjoin]] "

  retrieve
  " Returns the first `n' elements of `xs' which pass `f'.
    See also [[firstn]] [[keep]] "

  dedup " Returns `xs' sans duplicates. "

  single " Determines if `x' is a list with only one element. "

  intersperse " Inserts `x' between elements of `ys'. "

  counts
  " Returns a table with elements of `seq' as keys and the number of
    occurences of that element as values. "

  commonest
  " Returns a two-element list containing the most common element of
    `seq' and the number of times it occured in the sequence. "

  reduce
  " Applies `f' to an accumulated result on the elements of `xs'.
    Elements are processed left-to-right.
    `f' is applied to at most 2 elements at a time.
    See also [[rreduce]]
   "

  rreduce
  " Applies `f' to an accumulated result on the elements of `xs'
    Elements are processed right-to-left.
    `f' is applied to at most 2 elements at a time.
    See also [[reduce]] "

  parse-format
  " Parses a simple ~-format string. "

  prf
  " Prints according to a format string, replacing ~* with arguments. "

  load
  " Reads the expressions in `file' and evaluates them. "

  w/table " Creates a table assigned to `var' for use in `body'. "

  ero " Outputs `args' to stderr. "

  queue
  " Creates a queue.
    See also [[enq]] [[deq]] [[qlen]] [[qlist]] [[enq-limit]]"

  enq " Adds `obj' to a queue.  See also [[queue]] "

  deq " Removes and returns an item from a queue.  See also [[queue]] "

  qlen " Returns the number of items in a queue.  See also [[queue]] "

  qlist " Returns the queue contents as a list.  See also [[queue]] "

  enq-limit
  " Adds an item to the queue; removes a queue item if `limit' is
    exceeded.  See also [[queue]] "

  median " Computes the median of an unsorted list. "

  noisy-each
  " Performs `body' for each element of the sequence returned by `val',
    with each element assigned to `var'; prints a `.' every `n' elements. "

  point
  " Creates a form which may be exited by calling `name' from within `body'.
    See also [[catch]] "

  catch
  " Catches any value returned by `throw' within `body'.
    See also [[point]] "

  downcase
  " Converts `x' to lowercase, if a character, string, or symbol;
    otherwise, raises an error.
    See also [[upcase]] "

  upcase
  " Converts `x' to uppercase, if a character, string, or symbol;
    otherwise, raises an error.
    See also [[downcase]] "

  range "Return a range of numbers from `start' to `end', by `step'."

  mismatch " Returns the first index where `s1' and `s2' do not match. "

  memtable
  " Creates a membership table which returns t for each element in `ks' and
    nil otherwise. "

  w/bars
  " Prints out the strings printed by each expression in `body',
    separated by vertical bars (or whatever the value of `bar*' is). "

  thread
  " Launches the expressions in `body' in a new thread, returning the
    thread ID for that thread. "

  trav
  " Traverses an object `x'; each function in `fs' is applied to it, and
    sub-nodes of the object may be traversed by (self <node>) in any of the
    functions.
    See also [[treewise]] [[ontree]] "

  get
  " Returns a fn that calls it argument on `index'. "

  butlast
  " Returns a list containing every element of `x' but the last.
    See also: [[cut]] "

  between
  " As 'each, but runs `within' between each iteration of `body'.
    See also [[each]] "

  tofile
  " Redirects stdout to the file `name' within `body'. 
    See also [[fromfile]] [[w/outfile]] [[w/stdout]] "

  fromfile
  " Redirects standard input from the file `name' within `body'.
    See also [[tofile]] [[w/infile]] [[w/stdin]] "

  mapeach
  " Maps `(fn (,var) ,@body)' over `lst'.
    See also [[each]] [[map]] [[mappendeach]] "
  )
