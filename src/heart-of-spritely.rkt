#lang racket

(require goblins)
(require goblins/actor-lib/bootstrap)
(require goblins/actor-lib/methods)

(define vat (make-vat))

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
   (format "~\n** ~a **\n~a\n"
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

(define (spawn-blog-and-admin title)
  (define posts
    (spawn ^cell '()))
  (define (^blog _bcom)
    (methods
     [(get-title) title]
     [(get-posts) ($ posts 'get)]))
  (define (^admin bcom)
    (methods
     [(add-post post)
      (define current-posts
        ($ posts 'get))
      (define new-posts
        (cons post current-posts))
      ($ posts 'set new-posts)]))
  (define blog (spawn ^blog))
  (define admin (spawn ^admin))
  (values blog admin))

(define (spawn-post-and-editor #:title (title #f) #:author (author #f) #:body (body #f))
  ;; the public blogpost
  (define (^post _bcom)
    (methods
     [(get-content)
      (define data-triple
        ($ editor 'get-data))
      (cons '*post* data-triple)]))
  (define (^editor bcom title author body)
    (methods
     [(update #:title (title title) #:author (author author) #:body (body body))
      (bcom (^editor bcom title author body))]
     [(get-data)
      (list title author body)]))
  (define post (spawn ^post))
  (define editor (spawn ^editor title author body))
  (values post editor))


(vat 'run
     (lambda ()
       (define-values (post editor)
          (spawn-post-and-editor
           #:title "A day in the Park"
           #:author "Mikayla"
           #:body "la-di-da..."))

        (display-post post)

        ($ editor 'update #:body "it was a fine day...")))
