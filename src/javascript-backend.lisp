;;;; javascript-backend.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template)

(defclass javascript-backend (common-lisp-backend) ())


(defparameter *js-print-target* '$template-output$
  "Name variable for concatenate output strings")

(defparameter *default-js-namespace* '(ps:@ *closure-template *share)
  "Default JavaScript namespace")

(defvar *js-namespace* nil
  "Current JavaScript namespace")

(defparameter *check-js-namespace* t
  "If true and JavaScript namespace not exists - create namespace")

(defun js-string-to-symbol (str)
  (make-symbol (coerce (iter (for ch in-string str)
                             (when (upper-case-p ch)
                               (collect #\-))
                             (collect (char-upcase ch)))
                       'string)))

(defmethod backend-print ((backend javascript-backend) expr &optional directives)
  (list 'ps:+=
        *js-print-target*
        (case (or (getf directives :escape-mode)
                  (if *autoescape* :escape-html :no-autoescape))
          (:no-autoescape expr)
          (:escape-id `(encode-u-r-i-component ,expr))
          (:escape-uri `(encode-u-r-i ,expr))
          (:escape-html `(let ((val ,expr))
               (if (ps:== (ps:typeof val) "string")
                   ((ps:@ ((ps:@ ((ps:@ ((ps:@ ((ps:@ val replace) (ps:regex "/&/g") "&amp;") replace) (ps:regex "/</g") "&lt;") replace) (ps:regex "/>/g") "&gt;") replace) (ps:regex "/\"/g") "&quot;") replace) (ps:regex "/'/g") "&#039;")
                   val))))))


(defmethod translate-expression ((backend javascript-backend) expr)
  (if (and (consp expr)
           (symbolp (car expr)))
      (let ((key (car expr)))
        (case key
          (rem (cons 'ps:% (translate-expression backend
                                                 (cdr expr))))
          (equal (translate-expression backend
                                       (cons '= (cdr expr))))
          (not-equal (list 'ps:!=
                           (translate-expression backend (second expr))
                           (translate-expression backend (third expr))))
          (:round (translate-expression backend
                                        (cons 'round-closure-template
                                              (cdr expr))))
          (:variable (if (find (second expr) *local-variables* :test #'string=)
                         (make-symbol (symbol-name (second expr)))
                         `(ps:@ $data$ ,(make-symbol (string-upcase (second expr))))))
          (getf `(,@(let ((tr (translate-expression backend
                                                    (second expr))))
                         (cond ((symbolp tr) (list 'ps:@ tr))
                               ((and (listp tr)
                                     (eql (car tr) 'ps:@))
                                tr)
                               (t (list* 'ps:@ tr))))
                    ,(make-symbol (string-upcase (third expr)))))
          (otherwise (cons (or (find-symbol (symbol-name key)
                                            '#:closure-template)
                               (error "Bad keyword ~A" key))
                           (iter (for item in (cdr expr))
                                 (when item
                                   (collect (translate-expression backend item))))))))
      expr))

(defmethod translate-named-item ((backend javascript-backend) (item (eql 'closure-template.parser:namespace)) args)
  (let* ((*js-namespace* (if (car args)
                          (cons 'ps:@
                                (mapcar #'js-string-to-symbol
                                        (split-sequence:split-sequence #\. (car args))))
                          *default-js-namespace*)))
    (concatenate 'list
                 (if *check-js-namespace*
                     (nreverse (iter (for i from (length *js-namespace*) downto 2 )
                                     (collect (let ((part (subseq *js-namespace* 0 i)))
                                                (if (cddr part)
                                                    `(when (ps:== (ps:typeof ,part) "undefined")
                                                       (setf ,part (ps:create)))
                                                    `(when (ps:== (ps:typeof ,part) "undefined")
                                                       (defvar ,part (ps:create)))))))))
                 (iter (for fun in (cdr args))
                       (collect (translate-item backend
                                                fun))))))

(defun js-loop-variable-counter-symbol (loop-var)
  (make-symbol (format nil "~A-COUNTER" loop-var)))

(defun js-loop-sequence-symbol (loop-var)
  (make-symbol (format nil "~A-SEQ" loop-var)))

(defmethod translate-named-item ((backend javascript-backend) (item (eql 'closure-template.parser:template)) args)
  (let* ((*template-variables* nil)
         (body (let ((*autoescape* (if (find :autoescape (cdar args))
                                       (getf (cdar args) :autoescape)
                                       *autoescape*)))
                 (translate-item backend
                                 (cdr args)))))
    `(setf (,@*js-namespace* ,(js-string-to-symbol (caar args)))
         (lambda ($$data$$)
           (defvar $data$ (or $$data$$ (ps:create)))
           (defvar $template-output$ "")
           (macrolet ((has-data () '(if $$data$$ t))
                      (index (var) `,(js-loop-variable-counter-symbol var))
                      (is-first (var) `(= 0 (index ,var)))
                      (is-last (var) `(= (1- (ps:@ ,(js-loop-sequence-symbol var) length)) (index ,var)))
                      (round-closure-template (number &optional digits-after-point)
                        `(if ,digits-after-point
                             (let ((factor (expt 10.0 ,digits-after-point)))
                               (/ (round (* ,number factor)) factor))
                             (round ,number))))
             ,body)
           $template-output$))))


(defmethod translate-named-item ((backend javascript-backend) (item (eql 'closure-template.parser:foreach)) args)
  (let* ((loop-var (make-symbol (string-upcase (second (first (first args))))))
         (*local-variables* (cons loop-var
                                  *local-variables*))
         (seq-expr (translate-expression backend (second (first args))))
         (seqvar (js-loop-sequence-symbol loop-var))
         (counter (js-loop-variable-counter-symbol loop-var)))
    `(let ((,seqvar ,seq-expr))
       (if ,seqvar
           (progn
             (defvar ,counter 0)
             (dolist (,loop-var ,seqvar)
               ,(translate-item backend
                                (second args))
               (incf ,counter)))
           ,(if (third args)
                (translate-item backend
                                (third args)))))))

(defmethod translate-named-item ((backend javascript-backend) (item (eql 'closure-template.parser:switch-tag)) args)
  `(case ,(translate-expression backend (first args))
     ,@(iter (for clause in (cddr args))
             (collect (list (if (consp (first clause))
                                (iter (for i in (first clause))
                                      (collect (translate-expression backend i)))
                                (first clause))
                            (translate-item backend (cdr clause)))))
     ,@(if (second args) (list (list t
                                     (translate-item backend
                                                     (second args)))))))


(defmethod translate-named-item ((backend javascript-backend) (item (eql 'closure-template.parser:call)) args)
  (let ((fun-name `(,@*js-namespace* ,(js-string-to-symbol (first args))))
        (data-param (cond
                      ((eql (second args) :all) '$data$)
                      ((second args) (translate-expression backend (second args)))))
        (params (cddr args)))
    (if (not params)
        (backend-print backend
                       (if data-param
                           (list fun-name data-param)
                           (list fun-name))
                       (list :escape-mode :no-autoescape))
        (let ((call-expr '((defvar _$data$_ (ps:create)))))
          (when data-param
            (let ((lvar (gensym "$_")))
              (push `(ps:for-in (,lvar ,data-param)
                                (setf (aref _$data$_ ,lvar)
                                      (aref ,data-param ,lvar)))
                    call-expr)))
          (iter (for param in params)
                (let ((slotname `(ps:@ _$data$_ ,(make-symbol (symbol-name (second (second param)))))))
                  (push `(setf ,slotname "")
                        call-expr)
                  (if (third param)
                      (push `(setf ,slotname
                                   ,(translate-expression backend
                                                          (third param)))
                            call-expr)
                      (let ((*js-print-target* slotname))
                        (push (translate-item backend
                                              (cdddr param))
                              call-expr)))))
          `(progn ,@(reverse call-expr)
                  ,(backend-print backend
                                  (list fun-name '_$data$_)
                                  (list :escape-mode :no-autoescape)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; translate and compile template methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod translate-template ((backend (eql :javascript-backend)) template)
  (translate-template (make-instance 'javascript-backend)
                      template))

(defmethod compile-template ((backend (eql :javascript-backend)) template)
  (compile-template (make-instance 'javascript-backend)
                    template))

(defmethod compile-template ((backend javascript-backend) template)
  (with-output-to-string (out)
    (iter (for i in (translate-template backend template))
          (format out "~A~%" (ps:ps* i)))))

(defmethod compile-template ((backend javascript-backend) (templates list))
  (with-output-to-string (out)
    (iter (for template in templates)
          (iter (for i in (translate-template backend template))
                (format out "~A~%" (ps:ps* i))))))
