(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :net.acceleration.css-selectors.system)
    (defpackage :net.acceleration.css-selectors.system
      (:use :common-lisp :asdf))))

(in-package :net.acceleration.css-selectors.system)

(defsystem :css-selectors.recex-example-parser
    :description "An implementation of css selectors"
    :author "<programmers@acceleration.net>"
    :licence "LGPL (or talk to me)"
    :version "0.1"
    :components
    ((:module "src/recex" :serial T :components ((:file "recex"))))
    :depends-on (:recursive-regex :css))