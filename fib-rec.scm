(letrec ((fib-loop (lambda (n) (lambda (a) (lambda (b) (if (<= n 0) a (((fib-loop (- n 1)) b) (+ a b)))))))
         (fib (lambda (n) (((fib-loop n) 0) 1))))
  (display (fib 40)))
