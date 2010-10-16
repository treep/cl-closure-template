;;;;
;;;; asdf.lisp -- ASDF components for cl-closure-templates.
;;;;

(in-package :closure-template)

;;;; the `template-file' class

;;; note: that form and next one wants both `eval-when' forms (why?)
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass template-file (asdf:source-file)
    ((js-template
      :initform nil
      :initarg  :js-template
      :accessor js-template-of
      :type     (or symbol null)))
    (:documentation
     "This ASDF component defines COMPILE-OP and LOAD-SOURCE-OP operations that
take care of calling COMPILE-TEMPLATE on TEMPLATE-FILE component.")))

;;; we say that the `asdf::template-file' class is synonym for the
;;; `closure-template::template-file' class
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (find-class 'asdf::template-file) (find-class 'template-file)))

;;;; and methods

;;; KLUDGE: with this ASDF don't allow has `cl-source-file' and `template-file' components
;;; with the same names
;;; (defmethod asdf:source-file-type ((c template-file) (s asdf:module)) "tmpl")

(defmethod asdf:perform ((o asdf:compile-op) (c template-file))
  (let* ((package
          (symbol-package
           (compile-template :common-lisp-backend (asdf:component-pathname c))))
         (js-template
          (when (js-template-of c)
            (let ((symbol (intern (string (js-template-of c)) package)))
              (set symbol
                   (compile-template :javascript-backend (asdf:component-pathname c)))
              (export symbol package)))))
    (unless (or package js-template)
      (error 'asdf:operation-error :component c :operation o))))

(defmethod asdf:perform ((o asdf:load-op) (c template-file))
  (values))
