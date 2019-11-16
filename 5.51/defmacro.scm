(define macro (as-macro (lambda (a . b) (list 'as-macro (cons 'lambda (cons a b))))))
(define defmacro (lambda (nameargs . body) (cons 'define (list (car nameargs) (cons 'macro (cons (cdr nameargs) body))))))

