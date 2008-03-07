(load "lib/webscaff.arc")

;to start the blog, run (blog-start) and go to http://localhost:8080
;note that there is a freebee "hidden" are for viewing all comments at http://localhost:8080/comments that could be gated for comment administration activities.

(= blog (inst 'webscaff
	      'title "A Blog with Comments."))

;feature of webscaff allows init code to be put in this function if needed
(def blog-init ())

(inst-entity 'post blog
	     (title string)
	     (text text))

(inst-entity 'comment blog
	     (text text)
	     (post-id num))

;;link to posts from the front page
(defopr || req
  "posts")

;;add a link for creating a comment with each post
(def post-display (user p)
  (tag b (link (post-short-part p) (post-permalink p)))
  (when user
    (sp)
    (link "[edit]" (string "postedit?id=" (p 'id))))
  (sp)
  (link "[comments]" (string "postcomments?id=" (p 'id)))
  (br2)
  (pr (post-long-part p)))

;;tie new comments to a specific post
(mac postcommentpage (id title . body)
     `(whitepage 
       (center
	(widtable 600 
		  (tag b (link ,blog!title "../"))
		  (br 3)
		  ,title
		  (br 3)
		  ,@body
		  (br 3)
		  (w/bars (link 'comment-archive)
			  (link "new comment for this post" (string "postcommentnew?id=" ,id)))))))

;;view the comments for a specific post
(defop postcomments req
  (let id (arg req "id")
    (aif post.id
	 (let user (get-user req)
	   (postcommentpage id
			    (pr:post-short-part it)
			    (each c comments*
				  (when (is c!post-id id)
				    (comment-display user c)
				    (br 3)))))
	 (post-notfound))))

;;hide the post-id when editing the comment
(defopl postcommentnew req
  (let post-id (arg req "id")
    (whitepage
     (with (p (inst 'comment 'post-id post-id)
	      user (get-user req))
       (vars-form user
		  `((text text ,(p 'text) t t)
		    (num post-id ,(p 'id) nil nil))
		  (fn (name val) (= (p name) val))
		  (fn () 
		      (commentadd user p)
		      (whitepage (center (br 3)
					 (pr "comment created.")
					 (link "[back]" (string "postcomments?id=" post-id))))))))))

;;hide the post-id when viewing a comment
(def comment-long-part (p)
  "")

