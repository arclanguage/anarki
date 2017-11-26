; A blog for news.arc
; For the standalone blog example by PG, see blog.arc from arc3.1

(= postdir* (+ srvdir* "posts/")  blog-maxid* 0  posts* (table))

(deftem post  id nil  by nil  title nil  text nil)

(= blog-threshold* 10)

(def load-posts ()
  (each id (map int (dir postdir*))
    (= blog-maxid*      (max blog-maxid* id)
       (posts* id) (temload 'post (string postdir* id)))))

(def save-post (p) (temstore 'post p (string postdir* p!id)))

(def post (id) (posts* (errsafe:int id)))

(mac blogpage body
  `(longpage user (msec) nil "blog" "Blog" "blog"
     (center
       (widtable 600
          (center
            (w/bars
              (link "archive")
              (if (blogger user) (link "new post" "newpost"))
              (link "rss" "blog-rss")))
          (br)
         ,@body))))

(defop viewpost req (blogop post-page req))

(def blogop (f req)
  (aif (post (arg req "id"))
    (f (get-user req) it)
    (blogpage (pr "No such post."))))

(def blog-permalink (p) (string "viewpost?id=" p!id))

(def post-page (user p) (blogpage (display-post user p)))

(def display-post (user p)
  (tag b (link p!title (blog-permalink p)))
  (when (or (author user p)
            (admin user))
    (sp) (link "[edit]" (string "editpost?id=" p!id)))
  (br)
  (spanclass "subtext" (pr "by") (sp) (userlink user p!by))
  (br2)
  (tag (span "style" "font-family:serif; color:black;")
    (pr (md-from-form p!text))
    (br 4)
    (center (pr "***"))))

(def blogger (user)
  (and user
       (or (> (karma user) blog-threshold*)
           (admin user))))

(def newpost-page (user)
  (minipage "New post"
    (urform user req
      (blog-permalink (addpost user (arg req "t") (arg req "b")))
      (tab (row "title" (input "t" "" 60))
           (row "text"  (textarea "b" 10 80))
           (row ""      (submit))))))

(newsop newpost ()
  (if
    (no user)
      (login-page "You have to be logged in to submit blog posts."
        (fn (user ip)
            (newpost-page user)))
    (blogger user)
      (newpost-page user)
    (pr (string "Sorry, you need " blog-threshold* " karma to submit blog posts."))))

(def addpost (user title text)
  ; TODO: this one is not even checking if there's any title yet
  (atlet p (inst 'post 'id (++ blog-maxid*) 'by user 'title title 'text text)
    (save-post p)
    (= (posts* p!id) p)))

(defopl editpost req (blogop edit-blog-page req))

(def edit-blog-page (user p)
  (if (or (author user p)
          (admin user))
    (minipage "Edit post"
      (vars-form user
        `((string title ,p!title t t) (text text ,p!text t t))
         (fn (name val) (= (p name) val))
         (fn ()
           (save-post p)
           (post-page user p))))
    (pr (string "Sorry, you need " blog-threshold* " karma to edit posts."))))

(newsop archive ()
  (ensure-posts)
  (blogpage
    (tag ul
      (each p (map post (rev (range 1 blog-maxid*)))
        (tag li (link p!title (blog-permalink p)))))))

(newsop blog ()
  (ensure-posts)
    (blogpage
      (up i 0 5
        (awhen (posts* (- blog-maxid* i))
          (display-post user it)
          (br 3)))))

(def ensure-posts ()
  (ensure-dir postdir*)
  (load-posts))

(defop blog-rss req
  (ensure-posts)
  (if (bound 'rss-stories)
    (rss-stories
      (map [posts* _] (range 1 blog-maxid*))
      (+ this-site* " blog")
      (+ site-url* "blog"))))
