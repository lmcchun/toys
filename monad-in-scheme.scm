(define (make-tagged-value tag value)
  (cons tag value))

(define (get-tag x)
  (car x))

(define (get-value x)
  (cdr x))

(define (return a)
  (lambda (tag)
    (make-tagged-value tag a)))

(define (>>= m f)
  (lambda (tag)
    (let* ((m-result (m tag))
	   (tag (get-tag m-result))
	   (value (get-value m-result))
	   (new-m (f value)))
      (new-m tag))))

(define (inc n)
  (make-tagged-value (+ n 1) n))

(define (make-node value lchild rchild)
  (>>= inc
       (lambda (n)
	 (return (list (make-tagged-value n value) lchild rchild)))))

(define (make-tree depth)
  (if (= depth 0)
      (make-node depth '() '())
      (>>= (make-tree (- depth 1))
	   (lambda (lchild)
	     (>>= (make-tree (- depth 1))
		  (lambda (rchild)
		    (make-node depth lchild rchild)))))))
