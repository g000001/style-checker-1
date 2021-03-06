;;;; style-checker-1.lisp

(cl:in-package :style-checker-1-internal)

(def-suite style-checker-1)

(in-suite style-checker-1)

(defvar *style-checkers*
  ;; [sym => [checker-name => fctn] ...]
  (make-hash-table))

(defun clear-all-style-checkers ()
  (clrhash *style-checkers*))

(test clear-style-checkers
  ;;
  (is (zerop (hash-table-count (clear-style-checkers)))))

(defun put-style-checker (sym checker-name checker-fctn)
  (let ((checkers (gethash sym *style-checkers*)))
    (if checkers
        (setf (gethash checker-name checkers)
              checker-fctn)
        (setf (gethash sym *style-checkers*)
              (let ((newtab (make-hash-table)))
                (setf (gethash checker-name newtab)
                      checker-fctn)
                newtab)))))

(test put-style-checker
  (flet ((setup ()
           (clear-style-checkers)
           (put-style-checker 'foo 'foo-checker (lambda (form)
                                                  (declare (ignore form))
                                                  (format *error-output*
                                                          "hello")))))
    (setup))
  ;;
  (is (= 1 (hash-table-count *style-checkers*)))
  (is (string= "hello"
               (with-output-to-string (*error-output*)
                 (funcall (gethash 'foo-checker
                                   (gethash 'foo *style-checkers*))
                          'form)))))

(defun call-style-checkers (sym form)
  (or (consp form) (return-from call-style-checkers nil))
  (let ((tab (gethash sym *style-checkers*)))
    (when tab
      (maphash (lambda (k v)
                 (declare (ignore k))
                 (funcall v form))
               tab))))

(test call-style-checkers
  (flet ((setup ()
           (clear-style-checkers)
           (put-style-checker 'foo 'a (lambda (form)
                                        (declare (ignore form))
                                        (format *error-output* "A")))
           (put-style-checker 'foo 'b (lambda (form)
                                        (declare (ignore form))
                                        (format *error-output* "B")))
           (put-style-checker 'foo 'c (lambda (form)
                                        (declare (ignore form))
                                        (format *error-output* "C")))
           (put-style-checker 'foo 'd (lambda (form)
                                        (declare (ignore form))
                                        (format *error-output* "D")))
           (put-style-checker 'foo 'e (lambda (form)
                                        (declare (ignore form))
                                        (format *error-output* "E")))))
    (setup))
  ;;
  (is (null (set-difference (coerce "ABCDE" 'list)
                            (coerce (with-output-to-string (*error-output*)
                                      (call-style-checkers 'foo () ))
                                    'list))))
  ;;
  (is (null (call-style-checkers (gensym) (gensym)))))

(defun remove-style-checker (sym checker-name)
  (let ((checkers (gethash sym *style-checkers*)))
    (when checkers
      (remhash checker-name checkers))))

(test remove-style-checker
  (flet ((setup ()
           (clear-style-checkers)
           (put-style-checker 'foo 'a (lambda (form)
                                        (declare (ignore form))
                                        (format *error-output* "A")))
           (put-style-checker 'foo 'b (lambda (form)
                                        (declare (ignore form))
                                        (format *error-output* "B")))
           (put-style-checker 'foo 'c (lambda (form)
                                        (declare (ignore form))
                                        (format *error-output* "C")))
           (put-style-checker 'foo 'd (lambda (form)
                                        (declare (ignore form))
                                        (format *error-output* "D")))
           (put-style-checker 'foo 'e (lambda (form)
                                        (declare (ignore form))
                                        (format *error-output* "E")))))
    (setup))
  ;;
  (is (progn
        (remove-style-checker 'foo 'd)
        (null (set-difference (coerce "ABCE" 'list)
                              (coerce (with-output-to-string (*error-output*)
                                        (call-style-checkers 'foo () ))
                                      'list))))))

(defun clear-style-checker-suite (checker-name)
  (maphash (lambda (k tab)
             (declare (ignore k))
             (maphash (lambda (k v)
                        (declare (ignore v))
                        (when (eq checker-name k)
                          (remhash k tab)))
                      tab))
           *style-checkers*))


(test clear-style-checker-suite
  (let ((sym (gensym)))
    (flet ((setup ()
             (clear-style-checkers)
             (put-style-checker 'foo sym (lambda (form)
                                          (declare (ignore form))
                                          (format *error-output* "A")))
             (put-style-checker 'bar sym (lambda (form)
                                          (declare (ignore form))
                                          (format *error-output* "B")))
             (put-style-checker 'baz sym (lambda (form)
                                          (declare (ignore form))
                                          (format *error-output* "C")))
             (put-style-checker 'quux sym (lambda (form)
                                           (declare (ignore form))
                                           (format *error-output* "D")))))
      (setup))
    ;;
    (is-true (null (set-difference (coerce "ABCD" 'list)
                                   (coerce (with-output-to-string (*error-output*)
                                             (maphash (lambda (k v)
                                                        (declare (ignore k))
                                                        (maphash (lambda (k v)
                                                                   (when (eq k sym)
                                                                     (funcall v :ignore)))
                                                                 v))
                                                      *style-checkers*))
                                           'list))))
    ;; clear!
    (clear-style-checker-suite sym)
    (is-true (string= ""
                      (with-output-to-string (*error-output*)
                        (maphash (lambda (k v)
                                   (declare (ignore k))
                                   (maphash (lambda (k v)
                                              (when (eq k sym)
                                                (funcall v :ignore)))
                                            v))
                                 *style-checkers*))))))

;; eof