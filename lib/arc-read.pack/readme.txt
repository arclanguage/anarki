Arc-Read
--------

Written by Chris Hooper, released into the public domain.

Arc-Read is a reprogrammable implementation of the Arc reader, based
on the Lisp reader algorithm as described in the Common Lisp
specification:

http://www.lispworks.com/documentation/HyperSpec/Body/02_b.htm

Currently, Arc uses the Scheme reader, augmented by the syntax for
square brackets. However, there is a comment in the code indicating
that this is just a temporary situation. Arc-Read is intended to
provide a replacement, written in Arc, that can be customised with new
Arc syntax.

Arc-read can be customised in a way similar to the way that Lisp's
reader can. New macro functions and dispatching macros can be added to
extend or change Arc's syntax. It's also possible to change the
character syntax types, making it even more programmable than Lisp
(what this means and how to do it will be explained below).

Arc-read already has one extra feature that the standard reader
doesn't: the #. form can be used to execute arbitrary code at read
time (see below for an explanation of what this means). A few optional
features have also been provided in the Arc-Read package, but they are
not used by default, so as not to break existing Arc code. Again,
instructions for installing them can be found below.

Getting Started
---------------

Firstly, in order to change the reader of a running Arc session, you
need to do the following:

  (require "lib/custom-reader.arc")

You can then change the reader with the following command.

  (set-reader f)

where f is the new reader function. The reader should be a function
that accepts no required arguments, and one optional argument that
defaults to (stdin) (note the parens around stdin, these are
important). For example:

  (def f ((o port (stdin)))
    (blah ... blah ... blah))

To use arc-read, do the following:

  (require "pack.arc")
  (pack-add-path "./lib")
  (require 'arc-read)
  (set-reader arc-read)

You are now using arc-read as the R in your REPL. Hopefully, things
should not look too different, as arc-read defaults to being just like
the standard Arc reader (with the exception of adding the
sharpsign-dot syntax, explained below). To change back to the Scheme
reader, do this:

  (restore-reader)

What To Do If Things Go Wrong
-----------------------------

Arc-Read is undoubtably quite buggy, and if you try to install custom
syntax, things might get worse! So if things get screwed up and Arc
will no longer respond sensibly to anything you type, here's what to
do:

  1. Use control-C (or C-C C-C in Emacs) to escape from Arc into the
     Scheme REPL.
  2. Type (set-reader read) to restore the Scheme reader.
  3. Use (tl) to get back to the Arc top-level.
  4. Execute (= readtable* (copy standard-readtable*) 
                reader-macros* (copy standard-reader-macros*))
     to reset the readtables.

It should now be OK to switch the reader to arc-read again. If
problems persist then try reloading arc-read.arc, or maybe just try
switching it off and on again.

Uaing #. To Execute Code At Read Time
-------------------------------------

One feature that Lisp has, and Scheme (and therefore Arc) lacks is the
ability to execute code at read time using the #. syntax. Because
#. is invalid Scheme syntax (and is therefore never encountered in
valid Arc code) I have added it to Arc-Read. It can be removed by
customising the readtable as described below. #. can be used like
this:

  (= x 1)

  (let x 2
    #.(prn "The read-time value of x is: " x)
    (prn "The eval-time value of x is: " x))

Prepending #. to the beginning of a form causes the reader to execute
that code, and the resulting value is the value read. So if the above
code is executed, you should get this output:

  The read-time value of x is 1
  The eval-time value of x is 2

#. can be used for conditional reading (i.e. different code gets read
in depending on certain settings), or for installing new syntax
(which needs to be done before the rest of the code gets read) and
for many other things besides.

Extensions To Arc-Read
----------------------

The Arc-Read package comes with three extensions, intended to act as
examples of how the reader can be customised. To use them:

  (require "lib/extensions.arc")

This will load the code but it won't install them in the reader. The
first extension swaps the syntactic meaning of comma and dot, so that
you can use comma as a decimal point rather than a dot.

  (swap-commas-and-dots)
  
Arc-Read should now recognise 2,3 as a floating point number, and will
allow you to use dots in quasiquotes

  `(1 2 .(+ 1 2))
  => (1 2 3)

Unprocessed-strings adds C#-style unprocessed strings to Arc. An
unprocessed string is prepended by @, and escape sequences within it
are treated as normal characters.

  (unprocessed-strings)
  
  @"arc\libs\utils"
  => "arc\\libs\\utils"

This is obviously quite convenient for Windows programmers, and people
who use regular expressions! Note that one escape sequence is still
recognised:

  @"An escaped \" is still escaped"
  => "An escaped \" is still escaped"

Heredocs are arbitrary text documents embedded in Arc code. By
installing heredocs, you can build up complex strings over many lines,
with no processing of escape sequences (even \"). The heredocs syntax
consists of #< followed by a sequence of characters that will be used
to terminate the string. The heredoc begins on the next line:

#<EOF
This is a heredoc. We can put absolutely
any text in here, including " or \ or \n.
You could embed an entire XML page in Arc code using
this.
This particular heredoc is terminated by the string EOF,
however, it must appear on its own on a line or it will 
be ignored like all other characters.
EOF

Customising Arc-Read: Reader Macros
-----------------------------------

This is the interesting part, using Arc-Read to change Arc's
syntax. The simplest change to make is to add a reader macro. Reader
macros are functions that get executed whenever a certain character is
encountered by arc-read. For example, left and right parens are both
standard reader macros. When a left parenthesis is encountered, a
reader macro is called that will read in the next items and place them
in a list. By creating new reader macros, you can create new literal
types and syntax forms.

A reader macro is a function with the following signature:

  (def my-read-macro (port character) (blah...))

The function is bound to a character by adding an entry to the
reader-macros* table. For example, suppose we wanted to bind the above
function to the @ character, we would do this:

  (= (reader-macros* #\@) my-read-macro)

We also have to tell arc-read that @ is now a macro character. We do
this by changing the readtable.

  (= (readtable* #\@ 'term-macro))

Now, whenever arc-read encounters a @, it will call the my-read-macro
function to parse the input. It will pass the port being read and the
macro character that was encountered (in this case, @) as arguments to
the function. The result returned by the function will be the result
returned by arc-read.

One last twist: there are actually two different kinds of reader
macros: termintaing macros and non-terminating macros. The difference
is that non-terminating macros are allowed to occur within a symbol,
in which case they will be treated as normal characters, while
terminating macros aren't. # is an example of a non-terminating macro
character, while ( and ) are terminating macro characters. The above
example installed @ as a terminating macro. To make it
non-terminating, use this:

  (= readtable* #\@ 'non-term-macro)

Another last twist: I said the macro function would be executed
whenever the reader encountered the character (with the exception of
non-terminating characters in symbols). I lied. There are actually
other circumstances where the reader macro isn't called, such as
within a string. These exceptions should be pretty obvious.

Customising Arc-Read: Dispatching Characters
--------------------------------------------

Creating new reader macros is a pain because it potentially breaks
compatibility with standard Arc syntax. One alternative is to use a
sharpsign macro. The sharpsign (hash, pound, whatever) is reserved for
denoting special syntax. The behaviour of the sharpsign depends on the
character that follows it. For example, #. is used to execute
arbitrary code. #< is used in the heredocs example to read arbitrary
text. In Scheme #hash is used to denote a hash-table. It is called a
'dispatching character' because it dispatches to a reader macro
depending on the character that follows it.

To install new sharpsign syntax, add an entry to the
sharpsign-dispatch* table. For example, the heredocs extension
installs itself using the following code:

  (= (sharpsign-dispatch* #\<) heredoc-reader))

This binds the sequence #< to the heredoc-reader reader macro.

Customising Arc-Read: Character Syntax Types
--------------------------------------------

Every character has a syntax type, which determines how arc-read
processes it. We've already met terminating and non-terminating macro
characters. Here is a complete list of all syntax types:

term-macro:       terminating macro character
non-term-macro:   non-terminating macro character
single-escape:    used to escape the next character. The standard
                  single-escape character is \
multi-escape:     a pair are used to escape multiple characters. The
                  standard multi-escape character is |
alphabetic:       an ordinary letter with no special meaning
digit:            a digit
package-marker:   used to separate symbols and packages. None are
                  defined for Scheme, but : is used in Lisp
plus-sign:        used to indicate a positive number, + by default
minus-sign:       used to indicate a negative number, - by default
dot:              used to indicate a dot, not used
deciaml-point:    used to indicate a decimal point in a number
ratio-marker:     used to separate the numerator and denominator in a
                  ratio, / by default
exponent-marker:  used to indicate the exponential part of a float
invalid:          invalid in Arc syntax

The syntax type of a character can be changed by changing
readtable*. This kind of customisation (with the exception of macro
characters) is not possible in Common Lisp or Scheme, so consider it a
bonus.

Standard Tables
---------------
The tables standard-readtable* and standard-reader-macros* contain the
default configuration of Arc-Read. The variables readtable* and
reader-macros* are set to copies of these tables by default when
Arc-Read is first loaded. You can set the variables back again to
remove your custom syntax.

You should always copy the standard tables, rather than using them
directly, as they are the only way you can restore the reader back to
normal syntax. If it were possible to make them read-only, I would.

Bugs And Unit Tests
-------------------

If you find any bugs in Arc-Read, feel free to fix them.

Arc-Read comes with a set of unit tests. If you find a bug, please add
a test to cover that scenario, to ensure it doesn't happen again.

Cheers!
