;;;; float-controls.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

;;;; define RESTAS module

(restas:define-module #:example.float-controls
  (:use #:common-lisp
        ;; using package (a.k.a. namespace) from the `float-controls.tmpl' file
        #:example.float-controls.view))

(in-package #:example.float-controls)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pathname parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *top-dir*
  (merge-pathnames "example/"
                   (asdf:component-pathname (asdf:find-system '#:closure-template)))
  "closure-template example's directory")

(defparameter *dir* (merge-pathnames "float-controls/" *top-dir*)
  "float-controls directory")

(defparameter *jquery-dir* (merge-pathnames "jquery/" *top-dir*)
  "JQuery directory")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *name* "Ivan Petrov")

(defparameter *email* "Ivan.Petrov@example.com")

(defun with-json (&rest args)
  (list* :json (json:encode-json-plist-to-string args)
         args))

(defun name-to-json ()
  (with-json :value *name*
             :save-link (genurl 'save-name)))

(defun email-to-json ()
  (with-json :value *email*
             :save-link (genurl 'save-email)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; routes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-route file ("resources/:file")
  (merge-pathnames (format nil "resources/~A" file)
                   *dir*))

(define-route image ("resources/images/:file")
  (merge-pathnames (format nil "resources/images/~A" file)
                   *dir*))

(define-route jquery ("jquery/:file")
  (merge-pathnames (format nil "~A" file)
                   *jquery-dir*))

(define-route templates.js ("resources/templates.js"
                            :content-type "text/javascript")
  ;; this variable from a *.view package contain JS-code which was generetad by template-file
  *js-template*)

(define-route save-name ("api/name" 
                         :method :post 
                         :content-type "application/json"
                         :render-method #'json:encode-json-plist-to-string)
  (setf *name*
        (hunchentoot:post-parameter "value"))
  (name-to-json))

(define-route save-email ("api/email"
                          :method :post
                          :content-type "application/json"
                          :render-method #'json:encode-json-plist-to-string)
  (setf *email*
        (hunchentoot:post-parameter "value"))
  (email-to-json))

(define-route main (""
                    :render-method 'example.float-controls.view:page)
  (list :name (name-to-json)
        :email (email-to-json)))

;;;; start the site

(restas:start '#:example.float-controls :port 8080)
