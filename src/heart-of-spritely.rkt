#lang racket

(require goblins)
(require goblins/actor-lib/bootstrap)
(require goblins/actor-lib/methods)
(require goblins/utils/simple-sealers)

(provide display-post-content display-blog-header
         display-post display-blog
         spawn-blog-and-admin spawn-adminable-post-and-editor
         ^logger
         spawn-logged-revocable-proxy-pair
         spawn-post-guest-editor-and-reviewer)

(define (display-post-content post-content)
  (match post-content
    ((list '*post* post-title post-author post-body) ; Type Tagging
     (let* ((title (or post-title "<<No Title>>"))
            (title-underline (make-string (string-length title) #\=))
            (author (or post-author "<<Anonymous>>"))
            (body (or post-body "<<Empty blogpost!>>")))
       (display
        (format "~a\n~a\n By: ~a\n\n~a\n"
                title title-underline author body))))))

(define (display-blog-header blog-title)
  (define header-len
    (+ 6 (string-length blog-title)))
  (define title-stars
    (make-string header-len #\*))
  (display
   (format "~a\n** ~a **\n~a\n"
           title-stars blog-title title-stars)))

(define (display-post post)
  (display-post-content
   ($ post 'get-content)))

(define (display-blog blog)
  (display-blog-header
   ($ blog 'get-title))
  (for-each
   (lambda (post)
     (display "\n")
     (display-post post)
     (display "\n"))
   ($ blog 'get-posts)))



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
      (blog-sealer (list '*post-self-proof* post))]))
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

  (values
   (spawn ^blog)
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



(define (spawn-post-guest-editor-and-reviewer author blog-admin)
  (define-values (post editor)
    (spawn-adminable-post-and-editor blog-admin #:author author))

  (define submitted-already? (spawn ^cell #f))

  (define (ensure-not-submitted)
    (when ($ submitted-already?)
      (error "Already Submitted!")))

  (define (^reviewer _bcom)
    (methods
     [(approve)
      (ensure-not-submitted)
      ($ blog-admin 'add-post post)
      ($ submitted-already? #t)]))

  (define (^restricted-editor _bcom)
    (methods
     [(set-title new-title)
      (ensure-not-submitted)
      ($ editor 'update #:title new-title)]
     [(set-body new-body)
      (ensure-not-submitted)
      ($ editor 'update #:body new-body)]))

  (define reviewer (spawn ^reviewer))
  (define restricted-editor (spawn ^restricted-editor))
  (values post restricted-editor reviewer))
