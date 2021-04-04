(in-package :do-bang)


(defclass sequence-iterator ()
  ((iterator-sequence
     :accessor iterator-sequence)
   (iterator-state
     :accessor iterator-state)
   (limit-state
     :accessor limit-state)
   (from-end
     :accessor from-end)
   (iterator-step
     :accessor iterator-step)
   (iterator-endp
     :accessor iterator-endp)
   (iterator-element
     :accessor iterator-element)
   (iterator-setf-element
     :accessor iterator-setf-element)
   (iterator-index
     :accessor iterator-index)))


(defun make-iterator (sequence &rest initargs &key &allow-other-keys)
  (let ((instance (make-instance 'sequence-iterator)))
    (with-slots (iterator-state limit-state from-end iterator-step iterator-endp iterator-element
                 iterator-setf-element iterator-index)
                instance
      (setf (iterator-sequence instance) sequence)
      (multiple-value-setq (iterator-state limit-state from-end iterator-step iterator-endp
                            iterator-element iterator-setf-element iterator-index)
                           (apply #'sequence:make-sequence-iterator sequence initargs))
      instance)))


(defun iterator-el (instance)
  (funcall (iterator-element instance)
           (iterator-sequence instance)
           (iterator-state instance)))


(defun (setf iterator-el) (new-value instance)
  (funcall (iterator-setf-element instance)
           new-value
           (iterator-sequence instance)
           (iterator-state instance)))


(defun make-do (base decls end-form body)
  (let ((gen-vars (mapcar (lambda (decl &aux (var (first decl))
                                             (step-form (third decl)))
                             (when (or (listp var) ; create gensyms for all the iteration or multiple value declarations.
                                       (keywordp step-form))
                               (gensym)))
                           decls)))
    `(symbol-macrolet ,(mapcan (lambda (decl gen-var &aux (var (first decl))
                                                          (step-form (third decl)))
                                 (cond
                                   ((and (listp var)
                                         (eq :sequence step-form))
                                     `((,(first var) (funcall (iterator-index ,gen-var)
                                                              (iterator-sequence ,gen-var)
                                                              (iterator-state ,gen-var)))
                                       (,(second var) (iterator-el ,gen-var))))
                                   ((eq :sequence step-form)
                                     `((,var (iterator-el ,gen-var))))
                                   (gen-var ; For the multiple value declarations make a symbol macro that binds to nth
                                     (loop for sub in var
                                           for index from 0
                                           collect `(,sub (nth ,index ,gen-var))))))
                                decls
                                gen-vars)
       (,base ,(mapcar (lambda (decl gen-var &aux (var (first decl))
                                                  (init-form (second decl))
                                                  (step-form (third decl)))
                         (cond
                           ((eq :sequence step-form) ; iteration declaration
                             `(,gen-var (make-iterator ,init-form ,@(cdddr decl))
                                        (progn
                                          (setf (iterator-state ,gen-var)
                                                (funcall (iterator-step ,gen-var)
                                                         (iterator-sequence ,gen-var)
                                                         (iterator-state ,gen-var)
                                                         (from-end ,gen-var)))
                                          ,gen-var)))
                           ((and gen-var
                                 (cddr decl)) ; multiple value declation with a step form
                             `(,gen-var (multiple-value-list ,init-form) (multiple-value-list ,step-form)))
                           (gen-var ; multiple value declation with no step form
                             `(,gen-var (multiple-value-list ,init-form)))
                           (t ; normal declaration
                             decl)))
                       decls
                       gen-vars)
              ((or ,(first end-form)
                   ,@(mapcan (lambda (decl gen-var &aux (step-form (third decl)))
                               (when (eq :sequence step-form)
                                 `((funcall (iterator-endp ,gen-var)
                                            (iterator-sequence ,gen-var)
                                            (iterator-state ,gen-var)
                                            (limit-state ,gen-var)
                                            (from-end ,gen-var)))))
                              decls
                              gen-vars))
               ,@(cdr end-form))
            ,@body))))


(defmacro do! (decls end-form &body body)
  (make-do 'do decls end-form body))


(defmacro do!* (decls end-form &body body)
  (make-do 'do* decls end-form body))

                   
