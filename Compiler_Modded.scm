;;;Needs meta-eval functions

(define (compile exp target linkage penv)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage penv))
        ((assignment? exp)
         (compile-assignment exp target linkage penv))
        ((definition? exp)
         (compile-definition exp target linkage penv))
        ((if? exp) (compile-if exp target linkage penv))
        ((lambda? exp) (compile-lambda exp target linkage penv))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage
			   penv))
        ((cond? exp) (compile (cond->if exp) target linkage))
	((and (primitive-application? exp)
	      (eq? (find-variable (operator exp) 'not-found)))
	 (compile-primitive-application exp target linkage penv)) ;;modded
        ((application? exp)
         (compile-application exp target linkage penv))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (lexical-address-lookup addr env)
  (define (lex-search f-ind v-ind env)
    (let* ((frame (list-ref f-ind env))
	   (val (list-ref v-ind frame)))
      (if (eq? val '*unassigned*)
	  (errorf 'lexical-address-lookup
		  "Value unassigned - ~s" addr)
	  val)))
  (lex-search (car addr) (cdr addr) env))

(define (list-pair-ref addr lst)
  (if (= addr 0)
      lst
      (list-pair-ref (- addr 1) (cdr lst))))
(define (lexical-address-set! addr value env)
  (define (lex-search f-ind v-ind env)
    (let* ((frame (list-ref f-ind env))
	   (val-p (list-pair-ref v-ind frame)))
      (set-car! val-p value)))
  (lex-search (car addr) (cdr addr) env))
(define (find-variable var env)
  (define (locate f-ind v-ind vars env)
    (cond ((null? vars)
	   (if (null? env)
	       'not-found
	       (locate (+ f-ind 1) 0 (car env) (cdr env))))
	  ((eq? (car vars) var)
 	   (cons f-ind v-ind))
	  (else
	   (locate f-ind (+ v-ind 1) (cdr vars) env))))
  (if (null? env) 'not-found (locate 0 0 (car env) (cdr env))))


(define primitive-compile '(+ - / *)) ;;;MOD
(define (find-primitive op) op)
(define (primitive-application? exp) (memq (car exp) primitive-compile))

(define (spread-arguments operand-code1 operand-code2)
  (append-instruction-sequences
   operand-code1
   (preserving '(arg1)
	       operand-code2
	       (make-instruction-sequence
		'(arg1) '() '()))))


(define (compile-primitive-application exp target linkage penv)
  (let ((operator-code (find-primitive (operator exp)))
	(operand-code1 (compile (car (operands exp)) 'arg1 'next penv))
	(operand-code2 (compile (cadr (operands exp)) 'arg2 'next penv))
	(rest (cddr (operands exp))))
    (define (compile-prim-n operand-code1 operand-code2 rest)
      (if (null? rest)
	  (append-instruction-sequences
	   (spread-arguments operand-code1 operand-code2)
	   (make-instruction-sequence
	    '(arg1 arg2) '(val)
	    `((assign val (op ,operator-code) (reg arg1) (reg arg2)))))
	  (compile-prim-n
	   (append-instruction-sequences
	    (spread-arguments operand-code1 operand-code2)
	    (make-instruction-sequence
	     '(arg1 arg2) '(arg1)
	     `((assign arg1 (op ,operator-code) (reg arg1) (reg arg2)))))
	   (compile (car rest) 'arg2 'next penv)
	   (cdr rest))))
    (end-with-linkage
     linkage
     (compile-prim-n operand-code1 operand-code2 rest)
     )))




(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
          '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
          `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
   instruction-sequence
   (compile-linkage linkage)))


(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,exp))))))
(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,(text-of-quotation exp)))))))
(define (compile-variable exp target linkage penv)
  (end-with-linkage
   linkage
   (let ((addr (find-variable exp  penv)))
     (if (eq? addr 'not-found)
	 (make-instruction-sequence
	  '(env) (list target)
	  `((assign ,target
		    (op lookup-variable-value)
		    (const ,exp)
		    (reg env))))
	 (make-instruction-sequence
	  '(env) (list target)
	  `((assign ,target
		    (op lexical-address-lookup)
		    (const ,addr)
		    (reg env))))))))

(define (compile-assignment exp target linkage penv)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next penv)))
    (end-with-linkage
     linkage
     (preserving
      '(env)
      get-value-code
      (let ((addr (find-variable var penv)))
	(if (eq? var 'not-found)
	    (make-instruction-sequence
	     '(env val) (list target)
	     `((perform (op set-variable-value!)
			(const ,var)
			(reg val)
			(reg env))
               (assign ,target (const ok))))
	    (make-instruction-sequence
	     '(env val) (list target)
	     `((perform (op lexical-address-set!)
			(const ,addr)
			(reg val)
			(reg env))
               (assign ,target (const ok))))))))))
(define (compile-definition exp target linkage penv)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next penv)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

(define (compile-if exp target linkage penv)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))                    
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next penv))
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage))
            (a-code
             (compile (if-alternative exp) target linkage penv)))
        (preserving '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence '(val) '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))

(define (compile-sequence seq target linkage penv)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage penv)
      (preserving '(env continue)
       (compile (first-exp seq) target 'next penv)
       (compile-sequence (rest-exps seq) target linkage))))

(define (compile-lambda exp target linkage penv)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
         (make-instruction-sequence '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry penv))
       after-lambda))))

(define (extend-penv penv frame) (cons frame penv))
(define (compile-lambda-body exp proc-entry penv)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return (extend-penv formals penv)))))

(define (compile-application exp target linkage penv)
  (let ((proc-code (compile (operator exp) 'proc 'next penv))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next penv))
              (operands exp))))
    (preserving '(env continue)
     proc-code
     (preserving '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))
(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
          (car operand-codes)
          (make-instruction-sequence '(val argl) '(argl)
           '((assign argl
              (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
          (make-instruction-sequence '(proc argl)
                                     (list target)
           `((assign ,target
                     (op apply-primitive-procedure)
                     (reg proc)
                     (reg argl)))))))
       after-call))))

(define all-regs '(env proc val argl continue))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
           `((assign continue (label ,linkage))
             (assign val (op compiled-procedure-entry)
                         (reg proc))
             (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry)
                          (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
          '((assign val (op compiled-procedure-entry)
                        (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (errorf 'compile-proc-appl
		 "return linkage, target not val - ~s"
                 target))))

(define (registers-needed s)
  (if (symbol? s) '() (car s)))
(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))
(define (statements s)
  (if (symbol? s) (list s) (caddr s)))
(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))



(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))
(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg)
                          (registers-needed seq1))
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))

(define label-counter 0)
(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)
(define (make-label name)
  (string->symbol
    (string-append (symbol->string name)
                   (number->string (new-label-number)))))

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))
(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))





(compile
 '(define (factorial n)
    (define (iter product counter)
      (if (> counter n)
          product
          (iter (* counter product)
		(+ counter 1))))
    (iter 1 1))
 'val
 'next)
