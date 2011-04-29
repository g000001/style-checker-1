(in-package :sb-c)

(sb-ext:without-package-locks
  (defmacro def-ir1-translator (name (lambda-list start-var next-var result-var)
                                &body body)
    (let ((fn-name (symbolicate "IR1-CONVERT-" name))
          (guard-name (symbolicate name "-GUARD")))
      (with-unique-names (whole-var n-env)
        (multiple-value-bind (body decls doc)
                             (parse-defmacro lambda-list whole-var body name "special form"
                                             :environment n-env
                                             :error-fun 'compiler-error
                                             :wrap-block nil)
          `(progn
             (declaim (ftype (function (ctran ctran (or lvar null) t) (values))
                             ,fn-name))
             (defun ,fn-name (,start-var ,next-var ,result-var ,whole-var
                                         &aux (,n-env *lexenv*))
               (declare (ignorable ,start-var ,next-var ,result-var))
               ,@decls
               (style-checker-1:call-style-checkers ',name ,whole-var)
               ,body
               (values))
             #-sb-xc-host
             ;; It's nice to do this for error checking in the target
             ;; SBCL, but it's not nice to do this when we're running in
             ;; the cross-compilation host Lisp, which owns the
             ;; SYMBOL-FUNCTION of its COMMON-LISP symbols. These guard
             ;; functions also provide the documentation for special forms.
             (progn
               (defun ,guard-name (&rest args)
                 ,@(when doc (list doc))
                 (declare (ignore args))
                 (error 'special-form-function :name ',name))
               (let ((fun #',guard-name))
                 (setf (%simple-fun-arglist fun) ',lambda-list
                       (%simple-fun-name fun) ',name
                       (symbol-function ',name) fun)
                 (fmakunbound ',guard-name)))
             ;; FIXME: Evidently "there can only be one!" -- we overwrite any
             ;; other :IR1-CONVERT value. This deserves a warning, I think.
             (setf (info :function :ir1-convert ',name) #',fn-name)
             ;; FIXME: rename this to SPECIAL-OPERATOR, to update it to
             ;; the 1990s?
             (setf (info :function :kind ',name) :special-form)
             ',name))))))