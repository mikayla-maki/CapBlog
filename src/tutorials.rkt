#lang web-server/insta

(struct post (title body))

(define BLOG (list (post "First post!"
                         "First post content...")
                   (post "Second post..."
                         "Second post content...")))

(define (print-post post)
  `(div (span ,(string-append "title: " (post-title post)))
        (div ,(string-append "body: " (post-body post)))))

(define (start request)
  (response/xexpr
   `(html
     (head (title "my blog"))
     (body (h1 "under construction")
           (div ,@(map print-post BLOG)))
     )
   ))
