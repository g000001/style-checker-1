(in-package :sb-c)

(declaim (ftype (sfunction (ctran ctran (or lvar null) t) (values))
                ir1-convert))
(sb-ext:without-package-locks
;;;; IR1-CONVERT, macroexpansion and special form dispatching
  (macrolet (;; Bind *COMPILER-ERROR-BAILOUT* to a function that throws
             ;; out of the body and converts a condition signalling form
             ;; instead. The source form is converted to a string since it
             ;; may contain arbitrary non-externalizable objects.
             (ir1-error-bailout ((start next result form) &body body)
               (with-unique-names (skip condition)
                 `(block ,skip
                    (let ((,condition (catch 'ir1-error-abort
                                        (let ((*compiler-error-bailout*
                                               (lambda (&optional e)
                                                 (throw 'ir1-error-abort e))))
                                          ,@body
                                          (return-from ,skip nil)))))
                      (ir1-convert ,start ,next ,result
                                   (make-compiler-error-form ,condition
                                                             ,form)))))))

    ;; Translate FORM into IR1. The code is inserted as the NEXT of the
    ;; CTRAN START. RESULT is the LVAR which receives the value of the
    ;; FORM to be translated. The translators call this function
    ;; recursively to translate their subnodes.
    ;;
    ;; As a special hack to make life easier in the compiler, a LEAF
    ;; IR1-converts into a reference to that LEAF structure. This allows
    ;; the creation using backquote of forms that contain leaf
    ;; references, without having to introduce dummy names into the
    ;; namespace.
    (defun ir1-convert (start next result form)
      (ignore-errors
        (style-checker-1:call-style-checkers (car form) form))
      (ir1-error-bailout (start next result form)
                         (let* ((*current-path* (or (get-source-path form)
                                                    (cons (simplify-source-path-form form)
                                                          *current-path*)))
                                (start (instrument-coverage start nil form)))
                           (cond ((atom form)
                                  (cond ((and (symbolp form) (not (keywordp form)))
                                         (ir1-convert-var start next result form))
                                        ((leaf-p form)
                                         (reference-leaf start next result form))
                                        (t
                                         (reference-constant start next result form))))
                                 (t
                                  (ir1-convert-functoid start next result form)))))
      (values))

    ;; Generate a reference to a manifest constant, creating a new leaf
    ;; if necessary.
    (defun reference-constant (start next result value)
      (declare (type ctran start next)
               (type (or lvar null) result))
      (ir1-error-bailout (start next result value)
                         (let* ((leaf (find-constant value))
                                (res (make-ref leaf)))
                           (push res (leaf-refs leaf))
                           (link-node-to-previous-ctran res start)
                           (use-continuation res next result)))
      (values))) )




