(define (f _) (raise 42))

(define (g _) (handle (f '()) e (display e)))

(g '())

(define shift (let ((reifyP (lambda (p) (lambda (sk) (lambda (v) (push-prompt p (lambda (_) (push-subcont sk (lambda (dummy) v)))))))))
                (lambda (p) (lambda (f) (with-subcont p (lambda (sk) (push-prompt p (lambda (_) (f ((reifyP p) sk))))))))))
(define control (lambda (p) (lambda (f) (with-subcont p (lambda (sk) (push-prompt p (lambda (_) (f (lambda (v) (push-subcont sk (lambda (_) v)))))))))))

(define p (new-prompt))

(handle (push-prompt p (lambda (_) (raise 123))) e (display e))
; ((shift p) (lambda (k) k)) ; -> prompt not found

(let ((k (push-prompt p (lambda (_) (handle (let ((f ((shift p) (lambda (k) k)))) (* 3 (f '()))) e (+ 7 e))))))
  (display (k (lambda (_) ((shift p) (lambda (l) 4))))) ; -> 4
  (display (k (lambda (_) ((shift p) (lambda (l) (l 4)))))) ; -> 12
  (display (k (lambda (_) (raise 77)))) ; -> 84
  )
