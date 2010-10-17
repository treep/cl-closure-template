;;;; packages.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defpackage #:closure-template.parser.expression
  (:use #:cl #:iter)
  (:export #:parse-expression
           #:bad-expression-condition
           #:not-equal
           #:lispify-string))

(wiki-parser:define-parser #:closure-template.parser
  (:use #:cl #:iter #:closure-template.parser.expression)
  (:export #:parse-template))

(defpackage #:closure-template
  (:use #:cl #:iter)
  (:import-from #:closure-template.parser #:parse-template)
  (:import-from #:closure-template.parser.expression #:parse-expression #:not-equal #:lispify-string)
  (:export #:parse-template
           #:parse-expression
           #:translate-template
           #:compile-template
           #:*default-translate-package*
           #:*default-js-namespace*
           #:make-template-package
           #:escape-html
           #:encode-uri
           #:encode-uri-component
           #:decode-uri))
