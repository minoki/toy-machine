(let ((Z (lambda (f) ((lambda (x) (f (lambda (y) ((x x) y)))) (lambda (x) (f (lambda (y) ((x x) y))))))))
  (let ((fib-loop (Z (lambda (fib-loop) (lambda (n) (lambda (a) (lambda (b) (if (<= n 0) a (((fib-loop (- n 1)) b) (+ a b))))))))))
    (let ((fib (lambda (n) (((fib-loop n) 0) 1))))
      (display (fib 40)))))
