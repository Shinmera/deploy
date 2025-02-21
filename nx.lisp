(in-package #:org.shirakumo.deploy)

(setf *features* (delete :sb-core-compression *features*))

(defun interpret-compile (name &optional (definition (or (and (symbolp name) (macro-function name))
                                                         (fdefinition name))))
  (flet ((interpret ()
           (let* ((interpreted (eval definition))
                  (lambda (lambda (&rest args) (apply interpreted args))))
             (when name (setf (fdefinition name) lambda))
             ;; we do this indirection to satisfy COMPILED-FUNCTION-P.
             (values lambda NIL NIL))))
    (etypecase definition
      (sb-kernel:interpreted-function
       (cond (name
              (error "Can't compile an interpreted function with name ~a on NX." name)
              (values name NIL NIL))
             (T
              (interpret))))
      (function
       (setf (fdefinition name) definition)
       (values definition NIL NIL))
      (cons
       (interpret)))))

(define-hook (:build finalize-clos) ()
  (flet ((maybe-finalize (fun)
           (when (typep fun 'generic-function)
             (sb-pcl::set-funcallable-instance-function
              fun (sb-pcl::make-final-caching-dfun fun nil nil)))))
    (do-all-symbols (symbol)
      (when (fboundp symbol)
        (handler-case
            (maybe-finalize (fdefinition symbol))
          (error () (status 3 "Failed to finalize ~a" symbol)))))))

(define-hook (:boot stub-nx most-positive-fixnum) ()
  (status 1 "Stubbing out functions for the NX environment")
  (sb-ext:with-unlocked-packages ("CL" "SB-C")
    (setf (fdefinition 'cl:compile) #'interpret-compile)
    (setf (fdefinition 'cl:compile-file) (lambda (&rest args) (error "Can't COMPILE-FILE on the NX.")))
    (setf (fdefinition 'sb-c::%instance-typep) (lambda (instance type) (typep instance type)))
    (defun cl:user-homedir-pathname (&optional host)
      (declare (ignore host))
      (values
       (make-pathname :host sb-impl::*physical-host*
                      :device "save"
                      :directory '(:absolute)
                      :name NIL :type NIL :version :newest)))
    (setf sb-ext:*evaluator-mode* :interpret
          sb-impl::*physical-host* (sb-impl::make-win32-host)
          cl:*default-pathname-defaults* (sb-impl::intern-pathname
                                          sb-impl::*physical-host*
                                          "rom" '(:absolute) NIL NIL :newest))))
