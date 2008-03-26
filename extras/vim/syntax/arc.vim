" Vim syntax file
" Language:	Arc (Lisp)
" Last Change:	2008 Mar 03
" Maintainer: Shlomi Fish <shlomif@iglu.org.il>
" Original author:	Sergey Khorev <sergey.khorev@gmail.com>
" Original author:	Dirk van Deun <dirk@igwe.vub.ac.be>

" This script incorrectly recognizes some junk input as numerals:
" parsing the complete system of Scheme numerals using the pattern
" language is practically impossible: I did a lax approximation.
 
" MzScheme extensions can be activated with setting is_mzscheme variable

" Suggestions and bug reports are solicited by the author.

" Initializing:

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore

" Fascist highlighting: everything that doesn't fit the rules is an error...

syn match	arclispError	oneline    ![^ \t()\[\]";]*!
syn match	arclispError	oneline    ")"

" Quoted and backquoted stuff

syn region arclispQuoted matchgroup=Delimiter start="['`]" end=![ \t()\[\]";]!me=e-1 contains=ALLBUT,arclispStruc,arclispSyntax,arclispFunc

syn region arclispQuoted matchgroup=Delimiter start="['`](" matchgroup=Delimiter end=")" contains=ALLBUT,arclispStruc,arclispSyntax,arclispFunc
syn region arclispQuoted matchgroup=Delimiter start="['`]#(" matchgroup=Delimiter end=")" contains=ALLBUT,arclispStruc,arclispSyntax,arclispFunc

syn region arclispStrucRestricted matchgroup=Delimiter start="(" matchgroup=Delimiter end=")" contains=ALLBUT,arclispStruc,arclispSyntax,arclispFunc
syn region arclispStrucRestricted matchgroup=Delimiter start="#(" matchgroup=Delimiter end=")" contains=ALLBUT,arclispStruc,arclispSyntax,arclispFunc

" Popular Scheme extension:
" using [] as well as ()
syn region arclispStrucRestricted matchgroup=Delimiter start="\[" matchgroup=Delimiter end="\]" contains=ALLBUT,arclispStruc,arclispSyntax,arclispFunc
syn region arclispStrucRestricted matchgroup=Delimiter start="#\[" matchgroup=Delimiter end="\]" contains=ALLBUT,arclispStruc,arclispSyntax,arclispFunc

syn region arclispUnquote matchgroup=Delimiter start="," end=![ \t\[\]()";]!me=e-1 contains=ALLBUT,arclispStruc,arclispSyntax,arclispFunc
syn region arclispUnquote matchgroup=Delimiter start=",@" end=![ \t\[\]()";]!me=e-1 contains=ALLBUT,arclispStruc,arclispSyntax,arclispFunc

syn region arclispUnquote matchgroup=Delimiter start=",(" end=")" contains=ALL
syn region arclispUnquote matchgroup=Delimiter start=",@(" end=")" contains=ALL

syn region arclispUnquote matchgroup=Delimiter start=",#(" end=")" contains=ALLBUT,arclispStruc,arclispSyntax,arclispFunc
syn region arclispUnquote matchgroup=Delimiter start=",@#(" end=")" contains=ALLBUT,arclispStruc,arclispSyntax,arclispFunc

syn region arclispUnquote matchgroup=Delimiter start=",\[" end="\]" contains=ALL
syn region arclispUnquote matchgroup=Delimiter start=",@\[" end="\]" contains=ALL

syn region arclispUnquote matchgroup=Delimiter start=",#\[" end="\]" contains=ALLBUT,arclispStruc,arclispSyntax,arclispFunc
syn region arclispUnquote matchgroup=Delimiter start=",@#\[" end="\]" contains=ALLBUT,arclispStruc,arclispSyntax,arclispFunc

" R5RS Scheme Functions and Syntax:

if version < 600
  set iskeyword=33,35-39,42-58,60-90,94,95,97-122,126,_
else
  setlocal iskeyword=33,35-39,42-58,60-90,94,95,97-122,126,_
endif

syn keyword arclispSyntax fn and or if cond case def let with mac
syn keyword arclispSyntax do delay set! else =>
syn keyword arclispSyntax quote quasiquote unquote unquote-splicing
syn keyword arclispSyntax define-syntax let-syntax letrec-syntax syntax-rules
syn keyword arclispSyntax each for 

syn keyword arclispFunc no not boolean? is iso cons car cdr set-car!
syn keyword arclispFunc set-cdr! caar cadr cdar cddr caaar caadr cadar caddr
syn keyword arclispFunc cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr
syn keyword arclispFunc cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr
syn keyword arclispFunc cddaar cddadr cdddar cddddr null? list? list length
syn keyword arclispFunc append reverse list-ref memq memv member assq assv assoc
syn keyword arclispFunc symbol? symbol->string string->symbol number? complex?
syn keyword arclispFunc real? rational? integer? exact? inexact? = < > <= >=
syn keyword arclispFunc zero? positive? negative? odd? even? max min + * - / abs
syn keyword arclispFunc quotient remainder modulo gcd lcm numerator denominator
syn keyword arclispFunc floor ceiling truncate round rationalize exp log sin cos
syn keyword arclispFunc tan asin acos atan sqrt expt make-rectangular make-polar
syn keyword arclispFunc real-part imag-part magnitude angle exact->inexact
syn keyword arclispFunc inexact->exact number->string string->number char=?
syn keyword arclispFunc char-ci=? char<? char-ci<? char>? char-ci>? char<=?
syn keyword arclispFunc char-ci<=? char>=? char-ci>=? char-alphabetic? char?
syn keyword arclispFunc char-numeric? char-whitespace? char-upper-case?
syn keyword arclispFunc char-lower-case?
syn keyword arclispFunc char->integer integer->char char-upcase char-downcase
syn keyword arclispFunc string? make-string string string-length string-ref
syn keyword arclispFunc string-set! string=? string-ci=? string<? string-ci<?
syn keyword arclispFunc string>? string-ci>? string<=? string-ci<=? string>=?
syn keyword arclispFunc string-ci>=? substring string-append vector? make-vector
syn keyword arclispFunc vector vector-length vector-ref vector-set! procedure?
syn keyword arclispFunc apply map call-with-current-continuation
syn keyword arclispFunc call-with-input-file call-with-output-file input-port?
syn keyword arclispFunc output-port? current-input-port current-output-port
syn keyword arclispFunc open-input-file open-output-file close-input-port
syn keyword arclispFunc close-output-port eof-object? read read-char peek-char
syn keyword arclispFunc write display newline write-char call/cc
syn keyword arclispFunc list-tail string->list list->string string-copy
syn keyword arclispFunc string-fill! vector->list list->vector vector-fill!
syn keyword arclispFunc force with-input-from-file with-output-to-file
syn keyword arclispFunc char-ready? load transcript-on transcript-off eval
syn keyword arclispFunc dynamic-wind port? values call-with-values
syn keyword arclispFunc arclisp-report-environment null-environment
syn keyword arclispFunc interaction-environment

" ... so that a single + or -, inside a quoted context, would not be
" interpreted as a number (outside such contexts, it's a arclispFunc)

syn match	arclispDelimiter	oneline    !\.[ \t\[\]()";]!me=e-1
syn match	arclispDelimiter	oneline    !\.$!
" ... and a single dot is not a number but a delimiter

" This keeps all other stuff unhighlighted, except *stuff* and <stuff>:

syn match	arclispOther	oneline    ,[a-z!$%&*/:<=>?^_~+@#%-][-a-z!$%&*/:<=>?^_~0-9+.@#%]*,

" This seems to display errors on actually useful user-defined constructs
" so I'm remming it out.
"    -- Shlomi Fish
" syn match	arclispError	oneline    ,[a-z!$%&*/:<=>?^_~+@#%-][-a-z!$%&*/:<=>?^_~0-9+.@#%]*[^-a-z!$%&*/:<=>?^_~0-9+.@ \t\[\]()";]\+[^ \t\[\]()";]*,

syn match	arclispOther	oneline    "\.\.\."
syn match	arclispError	oneline    !\.\.\.[^ \t\[\]()";]\+!
" ... a special identifier

syn match	arclispConstant	oneline    ,\*[-a-z!$%&*/:<=>?^_~0-9+.@]*\*[ \t\[\]()";],me=e-1
syn match	arclispConstant	oneline    ,\*[-a-z!$%&*/:<=>?^_~0-9+.@]*\*$,
" Likewise as above
"syn match	arclispError	oneline    ,\*[-a-z!$%&*/:<=>?^_~0-9+.@]*\*[^-a-z!$%&*/:<=>?^_~0-9+.@ \t\[\]()";]\+[^ \t\[\]()";]*,

syn match	arclispConstant	oneline    ,<[-a-z!$%&*/:<=>?^_~0-9+.@]*>[ \t\[\]()";],me=e-1
syn match	arclispConstant	oneline    ,<[-a-z!$%&*/:<=>?^_~0-9+.@]*>$,
" Likewise as above
" syn match	arclispError	oneline    ,<[-a-z!$%&*/:<=>?^_~0-9+.@]*>[^-a-z!$%&*/:<=>?^_~0-9+.@ \t\[\]()";]\+[^ \t\[\]()";]*,

" Non-quoted lists, and strings:

syn region arclispStruc matchgroup=Delimiter start="(" matchgroup=Delimiter end=")" contains=ALL
syn region arclispStruc matchgroup=Delimiter start="#(" matchgroup=Delimiter end=")" contains=ALL

syn region arclispStruc matchgroup=Delimiter start="\[" matchgroup=Delimiter end="\]" contains=ALL
syn region arclispStruc matchgroup=Delimiter start="#\[" matchgroup=Delimiter end="\]" contains=ALL

" Simple literals:
syn region arclispString start=+\%(\\\)\@<!"+ skip=+\\[\\"]+ end=+"+

" Comments:

syn match	arclispComment	";.*$"


" Writing out the complete description of Scheme numerals without
" using variables is a day's work for a trained secretary...

syn match	arclispOther	oneline    ![+-][ \t\[\]()";]!me=e-1
syn match	arclispOther	oneline    ![+-]$!
"
" This is a useful lax approximation:
syn match	arclispNumber	oneline    "[-#+0-9.][-#+/0-9a-f@i.boxesfdl]*"
" syn match	arclispError	oneline    ![-#+0-9.][-#+/0-9a-f@i.boxesfdl]*[^-#+/0-9a-f@i.boxesfdl \t\[\]()";][^ \t\[\]()";]*!

syn match	arclispBoolean	oneline    "#[tf]"
syn match	arclispError	oneline    !#[tf][^ \t\[\]()";]\+!

syn match	arclispChar	oneline    "#\\"
syn match	arclispChar	oneline    "#\\."
syn match       arclispError	oneline    !#\\.[^ \t\[\]()";]\+!
syn match	arclispChar	oneline    "#\\space"
syn match	arclispError	oneline    !#\\space[^ \t\[\]()";]\+!
syn match	arclispChar	oneline    "#\\newline"
syn match	arclispError	oneline    !#\\newline[^ \t\[\]()";]\+!

if exists("b:is_mzscheme") || exists("is_mzscheme")
    " MzScheme extensions
    " multiline comment
    syn region	arclispComment start="#|" end="|#"

    " #%xxx are the special MzScheme identifiers
    syn match arclispOther oneline    "#%[-a-z!$%&*/:<=>?^_~0-9+.@#%]\+"
    " anything limited by |'s is identifier
    syn match arclispOther oneline    "|[^|]\+|"

    syn match	arclispChar	oneline    "#\\\%(return\|tab\)"

    " Modules require stmt
    syn keyword arclispExtSyntax module require dynamic-require lib prefix all-except prefix-all-except rename
    " modules provide stmt
    syn keyword arclispExtSyntax provide struct all-from all-from-except all-defined all-defined-except
    " Other from MzScheme
    syn keyword arclispExtSyntax with-handlers when unless instantiate define-struct case-lambda syntax-case
    syn keyword arclispExtSyntax free-identifier=? bound-identifier=? module-identifier=? syntax-object->datum
    syn keyword arclispExtSyntax datum->syntax-object
    syn keyword arclispExtSyntax let-values let*-values letrec-values set!-values fluid-let parameterize begin0
    syn keyword arclispExtSyntax error raise opt-lambda define-values unit unit/sig define-signature 
    syn keyword arclispExtSyntax invoke-unit/sig define-values/invoke-unit/sig compound-unit/sig import export
    syn keyword arclispExtSyntax link syntax quasisyntax unsyntax with-syntax

    syn keyword arclispExtFunc format system-type current-extension-compiler current-extension-linker
    syn keyword arclispExtFunc use-standard-linker use-standard-compiler
    syn keyword arclispExtFunc find-executable-path append-object-suffix append-extension-suffix
    syn keyword arclispExtFunc current-library-collection-paths current-extension-compiler-flags make-parameter
    syn keyword arclispExtFunc current-directory build-path normalize-path current-extension-linker-flags
    syn keyword arclispExtFunc file-exists? directory-exists? delete-directory/files delete-directory delete-file
    syn keyword arclispExtFunc system compile-file system-library-subpath getenv putenv current-standard-link-libraries
    syn keyword arclispExtFunc remove* file-size find-files fold-files directory-list shell-execute split-path
    syn keyword arclispExtFunc current-error-port process/ports process printf fprintf open-input-string open-output-string
    syn keyword arclispExtFunc get-output-string
    " exceptions
    syn keyword arclispExtFunc exn exn:application:arity exn:application:continuation exn:application:fprintf:mismatch
    syn keyword arclispExtFunc exn:application:mismatch exn:application:type exn:application:mismatch exn:break exn:i/o:filesystem exn:i/o:port
    syn keyword arclispExtFunc exn:i/o:port:closed exn:i/o:tcp exn:i/o:udp exn:misc exn:misc:application exn:misc:unsupported exn:module exn:read
    syn keyword arclispExtFunc exn:read:non-char exn:special-comment exn:syntax exn:thread exn:user exn:variable exn:application:mismatch
    syn keyword arclispExtFunc exn? exn:application:arity? exn:application:continuation? exn:application:fprintf:mismatch? exn:application:mismatch?
    syn keyword arclispExtFunc exn:application:type? exn:application:mismatch? exn:break? exn:i/o:filesystem? exn:i/o:port? exn:i/o:port:closed?
    syn keyword arclispExtFunc exn:i/o:tcp? exn:i/o:udp? exn:misc? exn:misc:application? exn:misc:unsupported? exn:module? exn:read? exn:read:non-char?
    syn keyword arclispExtFunc exn:special-comment? exn:syntax? exn:thread? exn:user? exn:variable? exn:application:mismatch?
    " Command-line parsing
    syn keyword arclispExtFunc command-line current-command-line-arguments once-any help-labels multi once-each 

    " syntax quoting, unquoting and quasiquotation
    syn region arclispUnquote matchgroup=Delimiter start="#," end=![ \t\[\]()";]!me=e-1 contains=ALL
    syn region arclispUnquote matchgroup=Delimiter start="#,@" end=![ \t\[\]()";]!me=e-1 contains=ALL
    syn region arclispUnquote matchgroup=Delimiter start="#,(" end=")" contains=ALL
    syn region arclispUnquote matchgroup=Delimiter start="#,@(" end=")" contains=ALL
    syn region arclispUnquote matchgroup=Delimiter start="#,\[" end="\]" contains=ALL
    syn region arclispUnquote matchgroup=Delimiter start="#,@\[" end="\]" contains=ALL
    syn region arclispQuoted matchgroup=Delimiter start="#['`]" end=![ \t()\[\]";]!me=e-1 contains=ALL
    syn region arclispQuoted matchgroup=Delimiter start="#['`](" matchgroup=Delimiter end=")" contains=ALL
endif


if exists("b:is_chicken") || exists("is_chicken")
    " multiline comment
    syntax region arclispMultilineComment start=/#|/ end=/|#/ contains=arclispMultilineComment

    syn match arclispOther oneline    "##[-a-z!$%&*/:<=>?^_~0-9+.@#%]\+"
    syn match arclispExtSyntax oneline    "#:[-a-z!$%&*/:<=>?^_~0-9+.@#%]\+"

    syn keyword arclispExtSyntax unit uses declare hide foreign-declare foreign-parse foreign-parse/spec
    syn keyword arclispExtSyntax foreign-lambda foreign-lambda* define-external define-macro load-library
    syn keyword arclispExtSyntax let-values let*-values letrec-values ->string require-extension
    syn keyword arclispExtSyntax let-optionals let-optionals* define-foreign-variable define-record
    syn keyword arclispExtSyntax pointer tag-pointer tagged-pointer? define-foreign-type
    syn keyword arclispExtSyntax require require-for-syntax cond-expand and-let* receive argc+argv
    syn keyword arclispExtSyntax fixnum? fx= fx> fx< fx>= fx<= fxmin fxmax
    syn keyword arclispExtFunc ##core#inline ##sys#error ##sys#update-errno

    " here-string
    syn region arclispString start=+#<<\s*\z(.*\)+ end=+^\z1$+
 
    if filereadable(expand("<sfile>:p:h")."/cpp.vim")
	unlet! b:current_syntax
	syn include @ChickenC <sfile>:p:h/cpp.vim
	syn region ChickenC matchgroup=arclispOther start=+(\@<=foreign-declare "+ end=+")\@=+ contains=@ChickenC
	syn region ChickenC matchgroup=arclispComment start=+foreign-declare\s*#<<\z(.*\)$+hs=s+15 end=+^\z1$+ contains=@ChickenC
	syn region ChickenC matchgroup=arclispOther start=+(\@<=foreign-parse "+ end=+")\@=+ contains=@ChickenC
	syn region ChickenC matchgroup=arclispComment start=+foreign-parse\s*#<<\z(.*\)$+hs=s+13 end=+^\z1$+ contains=@ChickenC
	syn region ChickenC matchgroup=arclispOther start=+(\@<=foreign-parse/spec "+ end=+")\@=+ contains=@ChickenC
	syn region ChickenC matchgroup=arclispComment start=+foreign-parse/spec\s*#<<\z(.*\)$+hs=s+18 end=+^\z1$+ contains=@ChickenC
	syn region ChickenC matchgroup=arclispComment start=+#>+ end=+<#+ contains=@ChickenC
	syn region ChickenC matchgroup=arclispComment start=+#>?+ end=+<#+ contains=@ChickenC
	syn region ChickenC matchgroup=arclispComment start=+#>!+ end=+<#+ contains=@ChickenC
	syn region ChickenC matchgroup=arclispComment start=+#>\$+ end=+<#+ contains=@ChickenC
	syn region ChickenC matchgroup=arclispComment start=+#>%+ end=+<#+ contains=@ChickenC
    endif

endif

" Synchronization and the wrapping up...

syn sync match matchPlace grouphere NONE "^[^ \t]"
" ... i.e. synchronize on a line that starts at the left margin

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_scheme_syntax_inits")
  if version < 508
    let did_scheme_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink arclispSyntax		Statement
  HiLink arclispFunc		Function

  HiLink arclispString		String
  HiLink arclispChar		Character
  HiLink arclispNumber		Number
  HiLink arclispBoolean		Boolean

  HiLink arclispDelimiter	Delimiter
  HiLink arclispConstant		Constant

  HiLink arclispComment		Comment
  HiLink arclispMultilineComment	Comment
  HiLink arclispError		Error

  HiLink arclispExtSyntax	Type
  HiLink arclispExtFunc		PreProc
  delcommand HiLink
endif

let b:current_syntax = "arclisp"
