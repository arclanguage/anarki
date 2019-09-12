($ (require db openssl))
; wrappers for db connections and queries
; https://docs.racket-lang.org/db/index.html

;(vconn (sqlite3-data-source #:database 'memory 
;                            #:mode 'create))
(mac vconn (data-source)
    `($ (virtual-connection
            (connection-pool
                (lambda () 
                    (displayln "connecting!")
                    (dsn-connect ,data-source))))))

(mac sqlite-connect (db mode (o busy-retry-limit 10) (o busy-retry-delay 0.1))
    `(vconn (sqlite3-data-source #:database ,db 
                                 #:mode ,mode
                                 #:busy-retry-limit ,busy-retry-limit
                                 #:busy-retry-delay ,busy-retry-delay)))

(mac postgresql-connect (user db (o password nil))
    `(vconn (postgresql-data-source #:user ,user 
                                    #:database ,db 
                                    #:password ,password)))

(mac postgresql-secure-connect (user db ssl-protocol (o passowrd nil))
    `(vconn (postgresql-data-source #:user ,user 
                                    #:database ,db 
                                    #:password ,password
                                    #:ssl 'yes 
                                    #:ssl-context (ssl-make-client-context ,ssl-protocol))))

(mac mysql-connect (user db (o password nil))
    `(vconn (mysql-data-source #:user ,user 
                               #:database ,db 
                               #:password ,password)))

(mac mysql-secure-connect (user db ssl-protocol (o password nil))
    `(vconn (mysql-data-source #:user ,user 
                               #:database ,db 
                               #:password ,password
                               #:ssl 'yes 
                               #:ssl-context (ssl-make-client-context ,ssl-protocol))))

(def disconnect (dbc)
    ($ (disconnect dbc)))

(def table-exists? (dbc t)
    ($ (table-exists? dbc t)))

(def list-tables (dbc)
    ($ (list-tables dbc)))

(def query (dbc stmt .args)
    ($ (apply query dbc (virtual-statement stmt) args)))

;(query-value conn "select 1 + $1" 3)
(def query-value (dbc stmt . args)
    ($ (apply query-value dbc (virtual-statement stmt) args)))

(def query-value? (dbc stmt . args)
    ($ (apply query-maybe-value dbc (virtual-statement stmt) args)))

(def query-exec (dbc stmt . args)
    ($ (apply query-exec dbc (virtual-statement stmt) args)))

(def query-exec-file (dbc path . args)
    (iflet sql (keep [< 0 (len _)] 
                (map trim 
                    (tokens 
                        (filechars (file-exists path)) #\;)))
        (each stmt sql ($ (apply query-exec dbc stmt args)))))

(def query-rows (dbc stmt . args) 
    ($ (apply query-rows dbc (virtual-statement stmt) args)))

(def query-rows? (dbc stmt . args) 
    ($ (apply query-maybe-row dbc (virtual-statement stmt) args)))

(def query-row (dbc stmt . args) 
    ($ (apply query-row dbc (virtual-statement stmt) args)))

(def query-list (dbc stmt . args) 
    ($ (apply query-list dbc (virtual-statement stmt) args)))

(def start-transaction (dbc)
    ($ (start-transaction dbc)))

(def commit-transaction (dbc)
    ($ (commit-transaction dbc)))
    
(def rollback-transaction (dbc)
    ($ (rollback-transaction dbc)))

(def prepare-statement (dbc stmt)
    ($ (prepare dbc stmt)))

(def bind-statement (pst params)
    ($ (bind-prepared-statement pst params)))

(def get-last-insert-id? (dbc)
    (query-value? dbc "SELECT last_insert_rowid()"))

(def get-last-column? (dbc column table)
    (query-value? dbc "SELECT $1 from $2 order by $1 DESC LIMIT 1" column table))

(def get-table-data (dbc table)
    (query-value? dbc "SELECT * from information_schema.columns where table_name = $1" table))




