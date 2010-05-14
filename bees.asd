(defpackage :bees-system
    (:use #:common-lisp #:asdf))

(in-package :bees-system)

(defsystem bees
  :name "bees"
  :description "Bee Colony Optimization algorithm."
  :author "Khokhlov Ivan"
  :licence "Public Domain"
  :version "1.0"
  :components (:file "bees"))
