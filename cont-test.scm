(let* ((p (new-prompt))
       (subcont (push-prompt p (lambda (_) (+ 3 (with-subcont p (lambda (subcont) subcont)))))))
  (display (push-subcont subcont (lambda (_) 5))))

(let* ((reifyP (lambda (p) (lambda (sk) (lambda (v) (push-prompt p (lambda (_) (push-subcont sk (lambda (dummy) v))))))))
       (shift (lambda (p) (lambda (f) (with-subcont p (lambda (sk) (push-prompt p (lambda (_) (f ((reifyP p) sk)))))))))
       (control (lambda (p) (lambda (f) (with-subcont p (lambda (sk) (push-prompt p (lambda (_) (f (lambda (v) (push-subcont sk (lambda (_) v)))))))))))
       (myabort (lambda (p) (lambda (x) (with-subcont p (lambda (_) x)))))
       (p (new-prompt)))
  (display (push-prompt p (lambda (_) 123)))
  (display (push-prompt p (lambda (_) (+ 1 ((shift p) (lambda (k) 42)))))))
