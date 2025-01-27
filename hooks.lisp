(in-package #:org.shirakumo.deploy)

(defvar *hooks* ())

(defclass hook ()
  ((name :initarg :name :initform (error "NAME required.") :accessor hook-name)
   (type :initarg :type :initform (error "TYPE required.") :accessor hook-type)
   (function :initarg :function :initform (constantly NIL) :accessor hook-function)
   (priority :initarg :priority :initform 0 :accessor hook-priority)
   (documentation :initarg :documentation :initform NIL :accessor docstring)))

(defmethod print-object ((hook hook) stream)
  (print-unreadable-object (hook stream :type T)
    (format stream "~a ~a" (hook-type hook) (hook-name hook))))

(defmethod documentation ((hook hook) type)
  (docstring hook))

(defmethod (setf documentation) (value (hook hook) type)
  (setf (docstring hook) value))

(defmethod documentation ((spec cons) (type (eql 'hook)))
  (docstring (or (apply #'hook spec) (error "No hook matching spec ~s" spec))))

(defmethod (setf documentation) (value (spec cons) (type (eql 'hook)))
  (setf (docstring (or (apply #'hook spec) (error "No hook matching spec ~s" spec))) value))

(defun hook (type name)
  (loop for hook in *hooks*
        do (when (and (eql type (hook-type hook))
                      (eql name (hook-name hook)))
             (return hook))))

(defun (setf hook) (hook type name)
  (let ((hooks (list* hook (remove-hook type name))))
    (setf *hooks* (sort hooks #'> :key #'hook-priority))
    hook))

(defun remove-hook (type name)
  (setf *hooks* (loop for hook in *hooks*
                      unless (and (eql type (hook-type hook))
                                  (eql name (hook-name hook)))
                      collect hook)))

(defmacro define-hook ((type name &optional (priority 0)) args &body body)
  (ecase type (:pre-load) (:load) (:build) (:deploy) (:boot) (:quit))
  (check-type name symbol)
  `(let ((,name (or (hook ,type ',name)
                    (make-instance 'hook :name ',name :type ,type))))
     (setf (hook-priority ,name) ,priority)
     (setf (hook-function ,name) (flet ((,name (&key ,@args &allow-other-keys)
                                          ,@body))
                                   #',name))
     (setf (hook ,type ',name) ,name)
     (setf (docstring ,name) ,(if (stringp (car body)) (car body)))
     ',name))

(defun run-hooks (type &rest args)
  (loop for hook in *hooks*
        do (when (eql type (hook-type hook))
             (restart-case (apply (hook-function hook) args)
               (report-error (&optional err)
                 :report "Print the error and continue running hooks."
                 (status 1 "Error during ~a ~a~@[: ~a~]" type (hook-name hook) err))))))

(defmacro define-resource-directory (name directory &key (copy-root T))
  `(define-hook (:deploy ,name) (system directory)
     (copy-directory-tree (merge-pathnames ,directory (asdf:system-source-directory system))
                          directory :copy-root ,copy-root)))
