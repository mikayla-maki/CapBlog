#lang web-server/insta

(require goblins
         goblins/actor-lib/bootstrap
         goblins/actor-lib/methods
         goblins/utils/simple-sealers
         goblins/ocapn
         goblins/actor-lib/joiners
         goblins/ocapn/netlayer/onion
         goblins/ocapn/netlayer/fake-intarwebs
         xml)


;(define-values (blog-ref)
;  (command-line #:args (blog-sref)
;                (string->ocapn-sturdyref blog-sref)))

(define-vat-run machine-run (make-vat))
(define mycapn
  (machine-run
   (define-values (onion-netlayer _onion-private-key _onion-service-id)
     (new-onion-netlayer))
   (spawn-mycapn onion-netlayer)))


(define (start req)
  (define blog-ref (string->ocapn-sturdyref
                    (extract-binding/single 'blog (request-bindings req))))

  (define web-goblins-channel (make-channel))

  (machine-run (define blog-vow (<- mycapn 'enliven blog-ref))
               (define get-blog-vow (<- blog-vow 'get-html-blog))
               (on get-blog-vow
                   (lambda (blog-content)
                     (channel-put web-goblins-channel blog-content))
                   #:catch
                   (lambda (err)
                     (channel-put web-goblins-channel (string-join
                                                       "<div>"
                                                       err
                                                       "</div>")))))

    (response/xexpr
        `(html (head (title "Fake blog service!"))
                         (body  ,(string->xexpr (channel-get web-goblins-channel))))))
