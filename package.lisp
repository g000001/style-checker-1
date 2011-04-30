;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :style-checker-1
  (:export :style-checker
           :clear-all-style-checkers
           :clear-style-checker-suite
           :put-style-checker
           :remove-style-checker
           :call-style-checkers))

(defpackage :style-checker-1-internal
  (:use :style-checker-1 :cl :fiveam))




