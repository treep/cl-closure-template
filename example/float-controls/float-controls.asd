;;;;
;;;; float-controls.asd -- define ASDF system.
;;;;

;;; Load closure-template system first because next `defsystem' definition use the
;;; `template-file' key in `components' form.
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (operate 'load-op :closure-template))

(defsystem float-controls
  :depends-on (#:closure-template #:parenscript #:restas #:cl-json)
  :components ((:template-file "float-controls.tmpl"
                               ;; this be a variable in a *.view package
                               :js-template *js-template*)
               (:file "float-controls")))
