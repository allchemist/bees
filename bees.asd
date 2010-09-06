(defpackage :jdx-system
    (:use :cl :asdf))

(in-package :jdx-system)

(defsystem cl-chem
  :name "cl-chem"
  :description "Chemical compounds identifier through IR spectra"
  :author "Khokhlov Ivan"
  :licence "BSD"
  :depends-on (split-sequence gplt cl-openbabel cl-store)
  :components
  ((:file "package")
   (:file "jdx" :depends-on ("package"))
   (:file "nist" :depends-on ("package"))
   (:file "compound" :depends-on ("package" "jdx" "nist"))))
