#lang scribble/manual
@(require
   (for-label
     racket/base
     (only-in racket/contract/base -> any any/c or/c)
     anarki))


@title{Anarki: Community-Managed Arc Variant}

@defmodule[anarki]

This module makes it possible to load scripts made for Anarki from
Racket.

Documentation on Anarki as a language is available at
@hyperlink["https://arclanguage.github.io/"]{the Anarki website}.


@table-of-contents[]


@section[#:tag "quick"]{Quick Experimentation}


@defproc[(anarki) (or/c null 'done)]{

  An easy entrypoint to the Anarki REPL from the Racket REPL.

  Specifically, this initializes the Anarki main namespace verbosely,
  then runs a REPL with @racket[current-directory] set to the Anarki
  installation directory and @racket[current-namespace] set to the
  Anarki main namespace. It's equivalent to the following:

  @racketblock[
    (anarki-init-in-main-namespace-verbose)
    (parameterize ([current-directory anarki-path]
                   [current-namespace anarki-main-namespace]
                   [current-readtable anarki-readtable])
      (anarki-repl))
  ]
}


@section[#:tag "init"]{Initializing}


@defproc*[(
  [(anarki-init) void?]
  [(anarki-init-verbose) void?]
  [(anarki-init-in-main-namespace) void?]
  [(anarki-init-in-main-namespace-verbose) void?]
)]{
  Initializes a namespace with the Arc primitives, so that it can be
  used to evaluate Arc code. This can take a while to complete.

  For the "@tt{-in-main-namespace}" variants, the namespace
  initialized is @racket[anarki-main-namespace], and the
  initialization is entirely skipped if either of these variants has
  been called before.

  For the other variants, the namespace initialized is
  @racket[(current-namespace)], and if the namespace has already been
  initialized, it's initialized again. This can even be used to
  double-initialize @racket[anarki-main-namespace].

  The "@tt{-verbose}" variants may print messages to
  @racket[(current-error-port)] to report on the initialization
  progress.
}


@section[#:tag "eval"]{Evaluating Code}


@defproc[(anarki-eval [expr any/c]) any]{
  Evaluates an s-expression of Arc code in
  @racket[(current-namespace)].
}

@defproc[(anarki-load [path path-string?]) void?]{
  Loads the given file of s-expressions as Arc code in
  @racket[(current-namespace)], reading them with
  @racket[(current-readtable)].
}

@defproc[(anarki-repl) (or/c null 'done)]{
  Begins an read-eval-print loop interacting over
  @racket[(current-input-port)] and @racket[(current-output-port)],
  reading s-expressions using @racket[(current-readtable)] and
  evaluating them as Arc code in @racket[(current-namespace)].

  This does not perform any initialization of the readtable or the
  namespace before it starts. For that purpose, use one of the
  variants of @racket[anarki-init].
}


@section[#:tag "context"]{Typical Context}


@defthing[anarki-readtable readtable?]{
  The readtable used for Arc code.
}

@defthing[anarki-main-namespace namespace?]{
  The namespace in which @racketmodfont{#lang} @racketmodname[anarki]
  modules are loaded. It's recommended to load Arc code into this
  namespace most of the time.

  @racketblock[
    (parameterize ([current-namespace anarki-main-namespace]
                   [current-readtable anarki-readtable])
      (anarki-load "my-file.arc"))
  ]
}

@defthing[anarki-path path?]{
  The system path to the directory where Anarki is installed. Most of
  Anarki's libraries are distributed in the @filepath{lib} directory
  under this path, and most of them expect @racket[current-directory]
  to be set to this path so they can load each other or store their
  application state.

  A complete News example:

  @racketblock[
    (anarki-init-in-main-namespace-verbose)
    (parameterize ([current-directory anarki-path]
                   [current-namespace anarki-main-namespace]
                   [current-readtable anarki-readtable])
      (anarki-load "lib/news.arc")
      (anarki-eval '(nsv)))
  ]
}
