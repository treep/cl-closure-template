;;;; common-lisp-backend.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; aux
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-template-output (&body body)
  `(cond
     (*template-output* ,@body)
     (t (let ((*template-output* (list "")))
          ,@body
          (apply #'concatenate
                 'string
                 (nreverse *template-output*))))))

(defmacro write-template-string (str)
  (let ((g-str (gensym)))
    `(let ((,g-str ,str))
       (push (cond
               ((typep ,g-str 'float) (let ((*read-default-float-format* (type-of ,g-str)))
                                        (format nil "~A" ,g-str)))
               (,g-str (format nil "~A" ,g-str)))
             *template-output*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implementataion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *template-variables* nil)

(defvar *local-variables* nil)

(defvar *loops-vars* nil)

(defun make-template-package (&optional (name "closure-template.share"))
  (let ((lispified-name (if (stringp name)
                            (lispify-string name)
                            name)))
    (or (find-package lispified-name)
        (eval `(defpackage ,lispified-name
                 (:use #:cl)
                 (:import-from #:closure-template #:*template-output*))))))

(defparameter *default-translate-package*
  (make-template-package))

(defclass common-lisp-backend () ())

(defun translate-variable (varkey)
  (let ((varsymbol (intern (symbol-name varkey))))
    (when (not (or (find varsymbol *local-variables*)
                   (find varkey *template-variables*)))
      (push varkey *template-variables*))
    varsymbol))

(defun +/closure-template (arg1 arg2)
  (if (or (stringp arg1)
          (stringp arg2))
      (format nil "~A~A" arg1 arg2)
      (+ arg1 arg2)))

(defun round/closure-template (number &optional digits-after-point)
  (if digits-after-point
      (let ((factor (expt 10.0 digits-after-point)))
        (/ (round (* number factor)) factor))
      (round number)))

(defmethod translate-expression ((backend common-lisp-backend) expr)
  (if (and (consp expr)
           (symbolp (car expr)))
      (let ((key (car expr)))
        (case key
          (:variable (translate-variable (second expr)))
          ('+ (translate-expression backend
                                    (cons '+/closure-template
                                          (cdr expr))))
          (:round (translate-expression backend
                                        (cons 'round/closure-template
                                              (cdr expr))))
          (otherwise (cons (or (find-symbol (symbol-name key)
                                            '#:closure-template)
                               (let ((s (find (symbol-name key)
                                              closure-template.parser.expression::*possible-functions*
                                              :key 'lispify-string
                                              :test #'string=)))
                                 (if s (intern (symbol-name key) *package*)))
                               (error "Bad keyword ~A" key))
                           (iter (for item in (cdr expr))
                                 (when item
                                   (collect (translate-expression backend item))))))))
      expr))


(defmethod backend-print ((backend common-lisp-backend) expr &optional directives)
  (case (or (getf directives :escape-mode)
            (if *autoescape* :escape-html :no-autoescape))
    (:no-autoescape `(write-template-string ,expr))
    (:escape-id `(write-template-string (encode-uri-component ,expr)))
    (:escape-uri `(write-template-string (encode-uri ,expr)))
    (:escape-html `(write-template-string (escape-html ,expr)))))


(defmethod translate-named-item ((backend common-lisp-backend) (item (eql 'closure-template.parser:namespace)) args)
  (let ((*package* (if (car args)
                       (make-template-package (car args))
                       *default-translate-package*)))
    (iter (for tmpl in (cdr args))
          (let ((symbol (intern (lispify-string (car (second tmpl))))))
            (export symbol)
            (proclaim (list 'ftype
                            'function
                            symbol))))
    (translate-item backend
                  (cdr args))))

(defun has-data-used-p (body)
  (cond
    ((and (consp body)
          (eql (car body) 'has-data)) t)
    ((consp body)
     (iter (for item in body)
           (finding t such-that (has-data-used-p item))))
    (t nil)))

(defun call-data-used-p (body)
  (cond
    ((and (consp body)
          (eql (car body) 'closure-template.parser:call)
          (eql (third body) :all))
     t)
    ((consp body)
     (iter (for item in body)
           (finding t such-that  (call-data-used-p item))))
    (t nil)))

(defmethod translate-named-item ((backend common-lisp-backend) (item (eql 'closure-template.parser:template)) args)
  (let* ((*template-variables* nil)
         (body (let ((*autoescape* (if (find :autoescape (cdar args))
                                       (getf (cdar args) :autoescape)
                                       *autoescape*)))
                 (translate-item backend
                                 (cdr args))))
         (binds (iter (for var in *template-variables*)
                      (collect (list (find-symbol (symbol-name var) *package*)
                                     `(getf $data$ ,var))))))
    `(defun ,(intern (lispify-string (caar args))) (,@(unless binds '(&optional)) $data$)       
       (declare ,@(if (and (not *template-variables*)
                           (not (has-data-used-p body))
                           (not (call-data-used-p args))
                          )
                     '((ignore $data$)))
                (optimize (debug 0) (speed 3)))
       (let ((*loops-vars* nil)
             ,@binds)
         (macrolet ((random-int (arg) `(random ,arg))
                    (has-data () '(not (null $data$)))
                    (index (s) `(second (assoc ',s *loops-vars*)))
                    (is-first (s) `(= 0 (index ,s)))
                    (is-last (s) (let ((var (gensym "$G")))
                                   `(let ((,var (assoc ',s *loops-vars*)))
                                      (= (second ,var)
                                         (third ,var))))))
           (with-template-output
             ,body))))))

(defmethod translate-named-item ((backend common-lisp-backend) (item (eql 'closure-template.parser:foreach)) args)
  (let* ((loop-var (intern (string-upcase (second (first (first args))))))
         (*local-variables* (cons loop-var
                                  *local-variables*))
         (seq-expr (translate-expression backend (second (first args))))
         (seqvar (gensym "$G")))
    `(let ((,seqvar ,seq-expr))
       (if ,seqvar
           (let ((*loops-vars* (acons ',loop-var (list 0 (1- (length ,seqvar)))
                                      *loops-vars*)))
             (map nil
                  (lambda (,loop-var)
                    ,(translate-item backend
                                    (second args))
                    (incf (index ,loop-var)))
                  ,seqvar))
           ,(if (third args)
                (translate-item backend
                                (third args)))))))

(defmethod translate-named-item ((backend common-lisp-backend) (item (eql 'closure-template.parser:literal)) args)
  `(write-template-string ,(car args)))


(defmethod translate-named-item ((backend common-lisp-backend) (item (eql 'closure-template.parser:if-tag)) args)
  (cond
    ((= (length args) 1) `(when ,(translate-expression backend
                                                       (first (first args)))
                            ,(translate-item backend
                                             (cdr (first args)))))
    ((and (= (length args) 2)
          (eql (first (second args)) t)) `(if ,(translate-expression backend
                                                                     (first (first args)))
                                              ,(translate-item backend
                                                               (cdr (first args)))
                                              ,(translate-item backend
                                                               (cdr (second args)))))
    (t (cons 'cond
             (iter (for v in args)
                   (collect (list (translate-expression backend
                                                        (first v))
                                  (translate-item backend
                                                  (cdr v)))))))))

(defmethod translate-named-item ((backend common-lisp-backend) (item (eql 'closure-template.parser:switch-tag)) args)
  (let* ((case-var (gensym "$G"))
         (clauses (iter (for clause in (cddr args))
                        (collect `((find ,case-var (list ,@(first clause)) :test #'equal) ,(translate-item backend
                                                                                                           (cdr clause)))))))

    `(let ((,case-var ,(translate-expression backend
                                             (first args))))
       (cond
         ,@clauses
         ,@(if (second args) (list (list t
                                         (translate-item backend
                                                         (second args)))))))))

(defmethod translate-named-item ((backend common-lisp-backend) (item (eql 'closure-template.parser:for-tag)) args)
  (let* ((loop-var (intern (string-upcase (second (first (first args))))))
         (*local-variables* (cons loop-var
                                  *local-variables*))
         (from-expr (translate-expression backend
                                          (second (second (first args)))))
         (below-expr (translate-expression backend
                                           (third (second (first args)))))
         (by-expr (translate-expression backend
                                        (fourth (second (first args))))))
    `(loop
        for ,loop-var from ,(if below-expr from-expr 0) below ,(or below-expr from-expr) ,@(if by-expr (list 'by by-expr))
        do ,(translate-item backend
                            (cdr args)))))


(defmethod translate-named-item ((backend common-lisp-backend) (item (eql 'closure-template.parser:call)) args)
  (let ((fun-name (or (find-symbol (lispify-string (first args)))
                      (error "Unknow template ~A" (first args)))))
    `(let ((data ,(cond
                   ((eql (second args) :all) '$data$)
                   ((second args) (translate-expression backend
                                                        (second args))))))
       ,@(iter (for param in (cddr args))
               (collect (list 'push
                              (if (third param)
                                  (translate-expression backend
                                                        (third param))
                                  `(let ((*template-output* nil))
                                     (with-template-output
                                       ,(translate-item backend
                                                        (cdddr param)))))
                              'data))
               (collect (list 'push
                              (intern (string-upcase (second (second param))) :keyword)
                              'data)))
       (let ((*autoescape* nil))
         (,fun-name data)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; translate and compile template methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod translate-template ((backend (eql :common-lisp-backend)) template)
  (translate-template (make-instance 'common-lisp-backend)
                    template))

(defmethod compile-template ((backend (eql :common-lisp-backend)) template)
  (compile-template (make-instance 'common-lisp-backend)
                    template))

(defmethod compile-template ((backend common-lisp-backend) template)
  (eval (translate-template backend template)))

(defmethod compile-template ((backend common-lisp-backend) (templates list))
  (iter (for template in templates)
        (compile-template backend template)))
