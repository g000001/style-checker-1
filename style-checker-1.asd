;;;; style-checker-1.asd

(cl:in-package :asdf)

(defsystem :style-checker-1
  :serial t
  :components ((:file "package")
               (:file "style-checker-1")))

(defmethod perform ((o test-op) (c (eql (find-system :style-checker-1))))
  (load-system :style-checker-1)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :style-checker-1-internal :style-checker-1))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

