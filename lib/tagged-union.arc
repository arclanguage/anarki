; Tagged unions in Arc
; See http://arclanguage.org/item?id=7364

; Examples:
;
; (tunion binary-tree
;   branch (left  binary-tree?)
;          (right binary-tree?)
;   leaf   (value object))
;
; (= bt (branch (leaf 10) (branch (leaf 42) (leaf 43))))
;
; (tunion maybe
;   just    (value object)
;   nothing)
;
; (= mb (just 10))
;
; (tcase bt
;   (binary-tree
;     branch (do (prn left) (prn right))
;            (ero "Non-branch binary tree."))
;   (maybe
;     just    (prn value)
;     nothing (prn "Nothing at all."))
;   else
;     (ero "Unrecognized type!"))
;
; (tcase mb maybe
;   just    value
;   nothing nil
;           (ero "Unrecognized type!"))
;
; (vtype triple a b c)
; (= tr (triple 'alpha 'beta 'gamma))
;
; (vtype vec3d  (x int?//num?) (y int?//num?) (z int?//num?))
; (= v3 (vec3d 1 2 3))
;
; (vcase tr
;   triple (list a b c)
;   vec3d  (sqrt:apply + (map [* _ _] (list x y z)))
;          'error)

(require "ssyntaxes.arc")

(def always (x)
  " Returns a function which always returns `x'. "
  (fn __ x))

(def difference (set1 set2 (o fis is))
  " Computes `set' \\ `set2', where `set1' and `set2' are lists being treated
    like sets.  Equality is determined by `fis'.
    See also [[union]] "
  (mappend [if (find (par fis _) set2) nil (list _)] set1))

(def flat1 (xs)
  " Flattens the first set of parentheses in a nested list.
    See also [[flat]] "
  (rreduce [if (acons _1) (join _1 _2) (cons _1 _2)] xs nil))

(= mac-seval $)

(add-ssyntax-bottom
  $    (fn lists (apply map R lists))
  $    mac-seval)

; Allows us to write (object slot) for a slot that can take any value.  The
; procedure object returns true for all arguments.
(= object (always t))

(unless (bound 'tagged-unions*)     (= tagged-unions*     (table)))
(unless (bound 'tunion-no-variant*) (= tunion-no-variant* (uniq)))
(unless (bound 'tunion-same-name*)  (= tunion-same-name*  (uniq)))

; Internal
(def tu-variant (tu)
  ((rep tu) 'type))

(def tu-slots (tu)
  ((rep tu) 'slots))

; Allows us to define tagged unions.  Hooray!
(mac tunion (name . types)
  " Defines a tagged union.
    See also [[vtype]] [[tcase]]  "
  (= tagged-unions*.name (table))
  (with (names nil values nil
         type-testify   '(fn (_) (if (isa _ 'fn) _ (fn (x) (isa x _))))
         sref-internals nil)
    (each type-frag types
      ; Assemble the ('type name) pairs into two lists, so that we can iterate
      ; through them.
      (case (type type-frag)
        ; If it's a symbol, then we're defining a new name; add a new set of
        ; values, and go.
        sym
          (do
            (zap [cons type-frag _] names)
            (zap [cons nil       _] values))
        ; Otherwise, we're adding a value to an existing variant.
        cons
          ; I changed my mind about the order (now it's (name pred?) instead of
          ; (pred? name)), so I'm reversing it here.
          (zap [cons (rev type-frag) _] (car values))
        ; else
          (err "Invalid member of tagged union declaration.")))
    ; The adding of 'do is just so that we can define multiple things.
    (apply list 'do
      ; Referencing
      `(defcall ,name (self slot)
         (if (mem slot ((tagged-unions* ',name) (tu-variant self)))
           ((tu-slots self) slot)
           (err 'ref
                (string "Objects of type " ',name " do not have a slot "
                         "named `" slot "', so the `" slot "' slot of "
                         (annotate ',name self) " could not be read."))))
      (join
        ; Iterates over every variant/slots pair.
        (mapeach (variant slots) ($list names values)
          (if slots
            (each part slots
              (push (cadr part) (tagged-unions*.name variant))
              (push `(= ((tu-slots self) ',(cadr part)) value)
                    sref-internals)
              (push `(and (is (tu-variant self) ',variant)
                          (is key ',(cadr part))
                          ((,type-testify ,(car part)) value))
                    sref-internals))
            (push tunion-no-variant* (tagged-unions*.name variant)))
          (list 'do
            ; Defines a constructor (called "variant") to create a new object
            ; of (sub)type variant.  If the types do not match what is given
            ; (where any object other than a function denotes [isa _
            ; given-type], and a function is assumed to be a predicate), then
            ; the constructor errors.  Otherwise, the function returns an object
            ; such that (isa obj 'name) is true, as is
            ; (is ((rep obj) 'type) 'variant).  It happens that right now
            ; (isa ((rep obj) 'properties) 'table), and the slots are just keys
            ; in the hash table, but I reserve the right to change that.
            `(def ,variant ,(rev:$cadr slots)
               (if (and ,@(map [list (list type-testify (car _)) (cadr _)] slots))
                 (annotate ',name (obj
                   type  ',variant
                   slots (listtab
                           ; Constructs a list of the form
                           ; (('slot-name slot-name)), without function
                           ; application.
                           (list ,@(map [list 'list `(quote ,(cadr _)) (cadr _)] slots)))))
                 (err ',variant
                      (string "Violated the invariants for the slot values "
                              "when constructing an object.  The arguments were "
                              (tostring:write:list ,@(rev:$cadr slots)) "."))))
            ; Make name-variant and variant both legal names for the constructor.
            (list '= (coerce (string name "-" variant) 'sym) variant)
            ; Stringification, of the form name/variant : slot1 = value1,
            ; slot2 = value2
            `(redef coerce (x typ . args)
               (if (and (isa x ',name) (is (tu-variant x) ',variant) (is typ 'string))
                 (let substrs '()
                   (maptable
                     (fn (k v)
                       (zap
                         [cons (tostr k " = " v) _]
                         substrs))
                     (tu-slots x))
                   (string "#" ',name "/" ',variant "{" (sjoin ", " substrs) "}"))
                 (apply old x typ args)))))
        ; Setting the contents (in a list so that it can be `join'ed)
        (list
          `(redef sref (self value key)
             (if (isa self ',name)
               (if
                 ,@sref-internals
                 
                 (mem key (keys:tu-slots self))
                   (err 'sref
                        (string "Violated the invariant for the `" key "' slot of "
                                "the " ',name " object, " self " by trying to "
                                "set it to " value "."))
                 ; else
                   (err 'sref
                        (string "Objects of type " ',name " do not have a slot "
                                "named `" key "', so the `" key "' slot of "
                                self " could not be set to " value ".")))
               (old self value key))))))))

(mac tcase (var . condblocks)
  " Checks the possible variants of a tagged union.
    See also [[tunion]] [[vcase]] "
  (if (and condblocks (sym?:car condblocks))
    `(tcase ,var
            ,condblocks
            ,@(if (even:len condblocks) (list 'else (last condblocks)) nil))
    (givens varn    (uniq)
            haselse (and (>= (len condblocks) 2) (is (at condblocks -2) 'else))
            cbs     (if haselse (butlast:butlast condblocks) condblocks)
            elsel   (if haselse (list:last condblocks)       nil)
      `(let ,varn ,var
         (if
           ,@(flat1:if (some ~cons? cbs)
               (err 'tcase "Syntax error.")
               (mapeach cb cbs
                 (let (ttype . conds) cb
                   (aif (tagged-unions* ttype)
                     (withs (variants  (keys it)
                             subelse   (odd:len conds)
                             velsel    (if subelse (list:last conds) nil)
                             conds     (if subelse (pair:butlast conds) (pair conds))
                             gvariants ($car conds)
                             extras    (difference gvariants variants)
                             missings  (if subelse nil (difference variants gvariants)))
                       (each (xs msg) (list (list extras   "not part of")
                                            (list missings "missing for"))
                                
                         (if xs
                           (let n (len xs)
                             (prne 'tcase
                                   ": The " (plural n "variant") " "
                                   (comma-and-str xs)
                                   " " (if (is n 1) "is" "are") " " msg
                                   " the tagged union " ttype
                                   "."))))
                       `((isa ,varn ',ttype)
                          (if
                            ,@(flat1:map
                                (fn ((typ body))
                                  `((is (tu-variant ,varn) ',typ)
                                     (with (,@(flat1:map
                                                 [list _ `(,varn ',_)]
                                                 (rem tunion-no-variant*
                                                      it.typ)))
                                        ,body)))
                                conds)
                            ,@velsel)))
                     (err 'tcase
                          (string "The type " ttype " is not a tagged union."))))))
           ,@elsel)))))

(mac vtype (type . arguments)
  " Creates a \"tagged union\" with one variant, which is effectively a
    simple structure.
    See also [[tunion]] [[vcase]] "
  `(tunion ,type ,type ,@(map [if (sym? _) `(,_ object) _] arguments)))

(mac vcase (var . body)
  " Analagous to `tcase' for solitary variants.
    See also [[vtype]] [[tcase]] "
  `(tcase ,var
     ,@(flat1:map [if (is (len _) 2)
                    `((,(car _) ,(car _) ,@(cdr _)))
                    (cons 'else _)]
                  (pair body))))
