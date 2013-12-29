(define f
  (lambda ()
    (let ((iter 0))
      (lambda ()
        (begin
          (set! iter (+ iter 1))
          (display iter))))))
