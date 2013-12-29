(lambda (f)
  ((lambda (x) (f (lambda (v) ((x x) v))))
   (lambda (x) (f (lambda (v) ((x x) v))))))

(lambda (f)
  ((lambda (x) (lambda (v) ((f (x x)) v)))
   (lambda (x) (lambda (v) ((f (x x)) v)))))