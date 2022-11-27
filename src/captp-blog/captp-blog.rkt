#lang racket

(require goblins)
(require goblins/actor-lib/bootstrap)
(require goblins/actor-lib/methods)
(require goblins/utils/simple-sealers)
(require goblins/ocapn)
(require goblins/ocapn/netlayer/onion)
(require net/url)

(define (get-post-content post-content)
  (match post-content
    ((list '*post* post-title post-author post-body) ; Type Tagging
     (let* ((title (or post-title "--No Title--"))
            (title-underline (make-string (string-length title) #\=))
            (author (or post-author "--Anonymous--"))
            (body (or post-body "--Empty blogpost!--")))
       (format "~a<br/>~a<br/> By: ~a<br/><br/>~a<br/>"
               title title-underline author body)))))


(define (get-blog-header blog-title)
  (define header-len
    (+ 6 (string-length blog-title)))
  (define title-stars
    (make-string header-len #\*))
  (format "~a<br/>** ~a **<br/>~a<br/>"
          title-stars blog-title title-stars))

(define (get-post post)
  (get-post-content
   ($ post 'get-content)))

(define (get-html-blog blog)
  (string-join
   (list
    "<div>"
    (get-blog-header
     ($ blog 'get-title))
    (string-join (map
                  (lambda (post)
                    (format "~a<br/>" (get-post post)))
                  ($ blog 'get-posts)))
    "</div>")))


(define (spawn-post-and-editor-internal blog-sealer
                                        #:title (title #f)
                                        #:author (author #f)
                                        #:body (body #f))
  (define (^post _bcom)
    (methods
     [(get-content)
      (define data-triple
        ($ editor 'get-data))
      (cons '*post* data-triple)]
     [(get-sealed-editor)
      (blog-sealer (list '*editor* editor))]
     [(get-sealed-self)
      (blog-sealer (list '*post-self-proof* post))]
     [(get-html)
      (get-post post)]))
  (define (^editor bcom title author body)
    (methods
     [(update #:title (title title) #:author (author author) #:body (body body))
      (bcom (^editor bcom title author body))]
     [(get-data)
      (list title author body)]))
  (define post (spawn ^post))
  (define editor (spawn ^editor title author body))
  (values post editor))

(define (spawn-blog-and-admin title)
  (define-values (blog-seal blog-unseal blog-sealed?)
    (make-sealer-triplet))
  (define posts
    (spawn ^cell '()))

  (define (^blog _bcom)
    (methods
     [(get-title)
      title]
     [(get-posts)
      ($ posts)]
     [(get-html)
      (get-html-blog blog-obj)]
     ))
  (define (^admin bcom)
    (methods
     [(new-post-and-editor
       #:title (title #f)
       #:author (author #f)
       #:body (body #f))
      (define-values (post editor)
        (spawn-post-and-editor-internal
         blog-seal
         #:title title
         #:author author
         #:body body))
      (list post editor)]
     [(add-post post)
      (define current-posts
        ($ posts))
      (define new-posts
        (cons post current-posts))
      (define post-self-proof
        ($ post 'get-sealed-self))
      (match (blog-unseal post-self-proof)
        ((list '*post-self-proof* obj)
         (unless (eq? obj post)
           (error "Self-proof not for this post"))))
      ($ posts new-posts)]

     [edit-post
      (make-keyword-procedure
       (lambda (kws kw-vals post . args)
         (define sealed-editor
           ($ post 'get-sealed-editor))
         (define editor
           (match (blog-unseal sealed-editor)
             ((list '*editor* editor) editor)))
         (keyword-apply $ kws kw-vals editor 'update args)))]))
  (define blog-obj (spawn ^blog))
  (values
   blog-obj
   (spawn ^admin)))

(define spawn-adminable-post-and-editor
  (make-keyword-procedure
   (lambda (kws kw-vals admin . args)
     (define post-and-editor
       (keyword-apply $ kws kw-vals  admin 'new-post-and-editor args))
     (match post-and-editor
       [(list post editor) (values post editor)]))))

(define (^logger _bcom)
  (define log
    (spawn ^cell '()))
  (methods
   [(append-to-log username object success args)
    (define new-log-entry
      (list '*entry* 'user username 'object object 'allowed? success 'args args))
    (define current-log ($ log))
    (define new-log (cons new-log-entry current-log))
    ($ log new-log)]
   [(get-log)
    ($ log)]))

(define (spawn-logged-revocable-proxy-pair username object log)
  (define revoked?
    (spawn ^cell #f))

  (define (^proxy _bcom)
    (make-keyword-procedure
     (lambda (kws kw-vals . args)
       (cond (($ revoked?)
              ($ log 'append-to-log username object 'denied args)
              'denied)
             (else
              ($ log 'append-to-log username object 'ok args)
              (keyword-apply $ kws kw-vals object args))))))

  (define proxy (spawn ^proxy))
  (values proxy revoked?))

(define-vat-run machine-run (make-vat))
(define mycapn
  (machine-run
   (define-values (onion-netlayer _onion-private-key _onion-service-id)
     (new-onion-netlayer))
   (spawn-mycapn onion-netlayer)))

(define-values (blog admin admin-log) (machine-run
 (define-values (blog admin)
   (spawn-blog-and-admin "Test blog!"))
 (define admin-log (spawn ^logger))
 ($ admin 'add-post (car ($ admin 'new-post-and-editor
                            #:title "Hello World!"
                            #:author "Mikayla M"
                            #:body "This is the first blog post in a small OCap network!")))

 (define admin-sref ($ mycapn 'register admin 'onion))
 (display "admin sref: ")
 (displayln (format "~a\n" (url->string (ocapn-sturdyref->url admin-sref))))

 (define admin-log-sref ($ mycapn 'register admin-log 'onion))
 (display "admin-log sref: ")
 (displayln (format "~a\n" (url->string (ocapn-sturdyref->url admin-log-sref))))

 (define blog-sref ($ mycapn 'register blog 'onion))
 (display "Blog sref: ")
 (displayln (format "~a\n" (url->string (ocapn-sturdyref->url blog-sref))))

 (values blog admin admin-log)
))


(define (register-new-admin username)
  (machine-run
   (define-values (new-admin new-admin-revoked?)
     (spawn-logged-revocable-proxy-pair
      username
      admin
      admin-log))
   (define new-admin-sref ($ mycapn 'register new-admin 'onion))
   (display (format "~a's sref: " username))
   (displayln (format "~a\n" (url->string (ocapn-sturdyref->url new-admin-sref))))
   new-admin-revoked?))
