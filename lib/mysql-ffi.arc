; mysql-ffi.arc
; by jimm@io.com
; Arc FFI to the MySQL client library
;
; Usage:
;   - Open an connection and save it
;   - Call mysql-execute and mysql-select as often as you like
;     - mysql-select takes a fn as an argument and calls it for each row
;     - Each row passed in to fn is a list; each member is a properly coerced
;       Arc type (num, int, string, or nil)
;   - Call mysql-close to close the connection
;
; Example:
;   arc> (= mysql-include-dir* "/usr/local/mysql/include") ; this is the default
;   arc> (load "lib/mysql-ffi.arc")
;   In file included from gs1788.c:1:
;   /usr/local/mysql/include/mysql.h:125: warning: ISO C90 does not support 'long long'
;   arc> (= conn (mysql-connect "localhost" "username" "password" "database"))
;   #<cpointer>
;   arc> (mysql-execute conn "create table test (col1 varchar(255), col2 int, col3 int, col4 datetime);")
;   t
;   arc> (mysql-insert conn "test" '("col1_string" 2 nil "`now())")
;   t
;   arc> (mysql-select conn "select * from test" prall)
;   col1_string, 2, nil, 2008-06-27 07:29:33#<void>
;   arc> (mysql-execute conn "drop table test")
;   t
;   arc> (mysql-close conn)
;   #<void>
;
; NOTE: mysql-insert treats strings whose first character is a backquote
; specially. After stripping off the backquote, those strings are not
; modified when turned into SQL (for example, they are not surrounded by
; single quotes). In the example above, "`now()" is quoted so that it is
; not turned into a SQL string but instead is passed to the SQL as "now()"
; (without the double quotes).
;
; NOTE: The "#<void> at the end of the select output is the return value of
; mysql-select.
;
; NOTE: mysql-include-dir* must be defined. If it is not defined when this
; file is loaded, then the default value mysql-default-include-dir* defined
; below is used.
;
; NOTE: when inline compiles, I see the following warning, which appears to be
; harmless so far:
;   In file included from gs1788.c:1:
;   /usr/local/mysql/include/mysql.h:125: warning: ISO C90 does not support 'long long'
;
; See http://www.informit.com/articles/article.aspx?p=30494&seqNum=6
;
; TODO
;   - cache the return from mysql-null-ptr
;   - cache the returns from mysql-nth-field-type for the results in a row
;   - more convenience functions, like mysql-create-table
;   - possibly a more lisp-like query language; not sure what that would look
;   like

(= mysql-default-include-dir* "/usr/local/mysql/include")

(= mysql-null-type* 6
   mysql-types* (listtab '((0 num)      ; MYSQL_TYPE_DECIMAL
                           (1 int)      ; MYSQL_TYPE_TINY
                           (2 int)      ; MYSQL_TYPE_SHORT
                           (3 int)      ; MYSQL_TYPE_LONG
                           (4 num)      ; MYSQL_TYPE_FLOAT
                           (5 num)      ; MYSQL_TYPE_DOUBLE
                           (6 sym)      ; MYSQL_TYPE_NULL
                           (7 string)   ; MYSQL_TYPE_TIMESTAMP,
                           (8 int)      ; MYSQL_TYPE_LONGLONG,
                           (9 int)      ; MYSQL_TYPE_INT24,
                           (10 string)  ; MYSQL_TYPE_DATE
                           (11 string)  ; MYSQL_TYPE_TIME,
                           (12 string)  ; MYSQL_TYPE_DATETIME
                           (13 int)     ; MYSQL_TYPE_YEAR,
                           (14 string)  ; MYSQL_TYPE_NEWDATE
                           (15 string)  ; MYSQL_TYPE_VARCHAR,
                           (16 int)     ; MYSQL_TYPE_BIT,
                           (246 MYSQL_TYPE_NEWDECIMAL) ; MYSQL_TYPE_NEWDECIMAL=246,
                           (247 string) ; MYSQL_TYPE_ENUM=247,
                           (248 MYSQL_TYPE_SET) ; MYSQL_TYPE_SET=248,
                           (249 string) ; MYSQL_TYPE_TINY_BLOB=249,
                           (250 string) ; MYSQL_TYPE_MEDIUM_BLOB=250,
                           (251 string) ; MYSQL_TYPE_LONG_BLOB=251,
                           (252 string) ; MYSQL_TYPE_BLOB=252,
                           (253 string) ; MYSQL_TYPE_VAR_STRING=253,
                           (254 string) ; MYSQL_TYPE_STRING=254,
                           (255 MYSQL_TYPE_GEOMETRY) ; MYSQL_TYPE_GEOMETRY=255
                           )))

; null_ptr idea is from code in Anarki's git.arc.
(w/inline 
  (string "#include \"" (varif mysql-include-dir* mysql-default-include-dir*) "/mysql.h\"
void* null_ptr() { return (void*)0; }
char* nth_string(char **array, int i) { return array[i]; }
/* return i'th field type enum (see mysql_com.h) */
int nth_field_type(MYSQL_FIELD* fields, int i) { return fields[i].type; }")
  (cdef mysql-null-ptr "null_ptr" cptr ())
  (cdef mysql-nth-string "nth_string" cstring (cptr cint))
  (cdef mysql-nth-field-type "nth_field_type" cint (cptr cint)))

(w/ffi "libmysqlclient"
  (cdef ffi-mysql-init "mysql_init" cptr (cptr))
  (cdef ffi-mysql-real-connect "mysql_real_connect" cvoid (cptr cstring cstring cstring cstring cint cstring cint))
  (cdef ffi-mysql-query "mysql_query" cint (cptr cstring))
  (cdef ffi-mysql-store-result "mysql_store_result" cptr (cptr))
  (cdef ffi-mysql-use-result "mysql_use_result" cptr (cptr))
  (cdef ffi-mysql-fetch-row "mysql_fetch_row" cptr (cptr))
  (cdef ffi-mysql-fetch-fields "mysql_fetch_fields" cptr (cptr))
  (cdef ffi-mysql-free-result "mysql_free_result" cvoid (cptr))
  (cdef ffi-mysql-num-fields "mysql_num_fields" cuint (cptr))
  (cdef ffi-mysql-close "mysql_close" cvoid (cptr))
)

(def mysql-init ()
  "Return MySQL connection object."
  (let conn (ffi-mysql-init (mysql-null-ptr))
    (if (is conn (mysql-null-ptr))
          (ero "Can not initialize MySQL connection."))
    conn))

(def mysql-connect-conn (conn host user pwd dbname (o port 3306) (o socket (mysql-null-ptr)))
  "Connects to a MySQL server using the specified connection."
  (let retval (ffi-mysql-real-connect conn host user pwd dbname port socket 0)
    (if (no retval)
          (ero "Can not connect to MySQL server."))))

(def mysql-connect (host user pwd dbname (o port 3306) (o socket (mysql-null-ptr)))
  "Creates a connection, connects to a MySQL server using it, and returns it."
  (let conn (mysql-init)
    (mysql-connect-conn conn host user pwd dbname port socket)
    conn))

(def mysql-execute (conn sql)
  "Executs one or more queries. Returns nil on error, else returns t."
  (let retval (ffi-mysql-query conn sql)
    (if (isnt 0 retval)
          (do
            (ero "Error running query " sql)
            nil)
        t)))

(def mysql-insert (conn table values)
  "Inserts VALUES into TABLE. Strings that start with a backquote
   are inserted literally, normal strings are escaped and quoted,
   symbols become strings, numbers stay as-is, and nil becomes NULL."
  (mysql-execute conn (string "insert into " table " values (" (tostring:prall (map [mysql-arc-to-sql _] values) "" ", ") ")")))

(def mysql-arc-to-sql (val)
  "Converts VAL into a value suitable for SQL. Strings that start with a
   backquote are inserted literally, normal strings are escaped and quoted,
   symbols become strings, numbers stay as-is, and nil becomes NULL."
  (let t (type val)
    (if (no val)
	  "NULL"
	(or (is t 'string) (is t 'sym))
	  (mysql-quote (coerce val 'string))
	(or (is t 'int) (is t 'num))
	  (string val))))

(def mysql-quote (s)
  "Returns string S as a value suitable for using in SQL. Strings
   that start with a backquote are inserted literally, nil is
   returned as the string \"NULL\", and everything else is escaped."
  (if (no s)
        "NULL"
      (or (empty s) (is "`" (cut s 0 1)))
        (cut s 1)
      (string "'" (subst "''" "'" s) "'")))

(def mysql-select (conn sql (o f idfn))
  "Executes SQL and calls FN for every returned row."
  (if (mysql-execute conn sql)
    (withs (result (ffi-mysql-use-result conn)
            fields (ffi-mysql-fetch-fields result))
      (whilet row (mysql-fetch-row result fields)
        (f row))
      (ffi-mysql-free-result result))))

(def mysql-gather-row (results fields row i n)
  "Given a row fetched from the database, returns a list of all
   columns in ROW, coerced to their proper types."
  (if (>= i n)
        (rev results)
      (mysql-gather-row (cons (mysql-nth-column fields row i) results) fields row (+ i 1) n)))

(def mysql-fetch-row (result fields)
  "Returns the next row from RESULT as a list."
  (let row (ffi-mysql-fetch-row result)
    (if (no row)
          nil
        (mysql-gather-row '() fields row 0 (ffi-mysql-num-fields result)))))

(def mysql-nth-column (fields row i)
  "Returns the i'th column value in row, coerced from string to the proper Arc
   type."
  (withs (str (mysql-nth-string row i)
          ft (mysql-nth-field-type fields i))
    (if (or (no str) (is mysql-null-type* ft))
          nil
        (coerce str (mysql-types* ft)))))

(def mysql-close (conn)
  "Closes the connection to the database."
  (ffi-mysql-close conn))
