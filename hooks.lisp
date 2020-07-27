#|
 This file is a part of deploy
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.deploy)

(defvar *hooks* ())

(defclass hook ()
  ((name :initarg :name :accessor hook-name)
   (type :initarg :type :accessor hook-type)
   (function :initarg :function :accessor hook-function)
   (priority :initarg :priority :accessor hook-priority))
  (:default-initargs
   :name (error "NAME required.")
   :type (error "TYPE required.")
   :function (constantly NIL)
   :priority 0))

(defmethod print-object ((hook hook) stream)
  (print-unreadable-object (hook stream :type T)
    (format stream "~a ~a" (hook-type hook) (hook-name hook))))

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
  (ecase type (:load) (:build) (:deploy) (:boot) (:quit))
  (check-type name symbol)
  `(let ((,name (or (hook ,type ',name)
                    (make-instance 'hook :name ',name :type ,type))))
     (setf (hook-priority ,name) ,priority)
     (setf (hook-function ,name) (flet ((,name (&key ,@args &allow-other-keys)
                                          ,@body))
                                   #',name))
     (setf (hook ,type ',name) ,name)
     ',name))

(defun run-hooks (type &rest args)
  (loop for hook in *hooks*
        do (when (eql type (hook-type hook))
             (restart-case (apply (hook-function hook) args)
               (report-error (err)
                 :report "Print the error and continue running hooks."
                 (status 1 "Error during ~a: ~a" type err))))))

(defmacro define-resource-directory (name directory &key (copy-root T))
  `(define-hook (:deploy ,name) (system directory)
     (copy-directory-tree (merge-pathnames ,directory (asdf:system-source-directory system))
                          directory :copy-root ,copy-root)))
