(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
         (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
  (begin (display (even? 100))
         (display (even? 101))
         (display (even? 1000))
         (display (even? 1001))))
