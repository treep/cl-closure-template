;;;; translate.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template)

(defvar *template-output* nil)

(defvar *autoescape* t)

(defgeneric translate-item (backend item))

(defgeneric translate-named-item (backend item args)
  (:method(backend item args)))


(defgeneric backend-print (backend expr &optional directives))

(defgeneric translate-expression (backend expr))

(defgeneric translate-template (backend template))

(defgeneric compile-template (backend template))

(defmethod translate-template (backend template)
  (translate-item backend
                  (parse-template template)))

(defparameter *substitions* '(closure-template.parser:space-tag
                              closure-template.parser:emptry-string
                              closure-template.parser:carriage-return
                              closure-template.parser:line-feed
                              closure-template.parser:tab
                              closure-template.parser:left-brace
                              closure-template.parser:right-brace))

(defmethod translate-item (backend (item cons))
  (cond
    ((and (symbolp (car item))
          (not (find (car item)
                     *substitions*))) (translate-named-item backend
                                                            (car item)
                                                            (cdr item)))
    ((= (length item) 0))
    ((= (length item) 1) (translate-item backend
                                         (car item)))
    (t (cons 'progn
             (iter (for i in item)
                   (let ((c (translate-item backend
                                            i)))
                     (when c
                       (collect c))))))))

(defmethod translate-item (backend (item symbol))
  nil)

(defmethod translate-item (backend (item string))
  (backend-print backend
                 item
                 '(:escape-mode :no-autoescape)))

(defmethod translate-item (backend (symbol symbol))
  (backend-print backend
                 (case symbol
                   (closure-template.parser:space-tag (string #\Space))
                   (closure-template.parser:emptry-string "")
                   (closure-template.parser:carriage-return (string #\Return))
                   (closure-template.parser:line-feed (string #\Newline))
                   (closure-template.parser:tab (string #\Tab))
                   (closure-template.parser:left-brace "{")
                   (closure-template.parser:right-brace "}")
                   (otherwise (call-next-method)))))

(defmethod translate-named-item (backend (item (eql 'closure-template.parser:print-tag)) args)
  (backend-print backend
                 (translate-expression backend (car args))
                 (list :escape-mode
                       (cond
                         ((getf (cdr args) :no-autoescape) :no-autoescape)
                         ((getf (cdr args) :id) :id)
                         ((getf (cdr args) :escape-html) :escape-html)
                         ((getf (cdr args) :escape-uri) :escape-uri)
                         ((getf (cdr args) :escape-js) :escape-js)))))
