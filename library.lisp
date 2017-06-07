#|
 This file is a part of deploy
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.deploy)

(defparameter *system-source-directories*
  (list #+windows #p"C:/Windows/system32/"
        #+(and windows x86) #p"C:/Windows/SysWoW64/"
        #+unix #p"/usr/lib/"
        #+unix #p"/usr/lib/*/"
        #+unix #p"/usr/lib64/"
        #+unix #p"/usr/local/lib/"
        #+darwin #p"/usr/local/Cellar/**/lib/"
        #+darwin #p"/opt/local/lib"))

(defun list-libraries ()
  (mapcar #'ensure-library (cffi:list-foreign-libraries)))

(defun ensure-library (library)
  (etypecase library
    (library library)
    (cffi:foreign-library
     (change-class library 'library))
    (symbol
     (ensure-library (cffi::get-foreign-library library)))))

(defclass library (cffi:foreign-library)
  ((system :initarg :system :initform NIL :accessor library-system)
   (sources :initarg :sources :initform () :accessor library-sources)
   (path :initarg :path :initform NIL :accessor library-path)
   (dont-open :initarg :dont-open :initform NIL :accessor library-dont-open-p)
   (dont-deploy :initarg :dont-deploy :initform NIL :accessor library-dont-deploy-p)))

(defmethod print-object ((library library) stream)
  (print-unreadable-object (library stream :type T)
    (format stream "~a" (library-name library))))

(defmethod possible-pathnames ((library library))
  (list (cffi:foreign-library-pathname library)
        (make-lib-pathname (format NIL "*~(~a~)*" (library-name library)))))

(defmethod find-source-file ((library library))
  (let ((sources (append (library-sources library)
                         (when (library-system library)
                           (discover-subdirectories
                            (asdf:system-source-directory
                             (library-system library))))
                         cffi:*foreign-library-directories*
                         *system-source-directories*)))
    (dolist (path (possible-pathnames library))
      (when path
        (loop with filename = (pathname-filename path)
              for source in sources
              for file = (merge-pathnames filename source)
              do (when (uiop:file-exists-p file)
                   (return-from find-source-file file)))))))

(defmethod shared-initialize :after ((library library) slots &key)
  (unless (library-path library)
    (setf (library-path library) (find-source-file library))))

(defmethod library-name ((library library))
  (cffi:foreign-library-name library))

(defmethod open-library ((library library))
  (cffi:load-foreign-library (library-name library)))

(defmethod close-library ((library library))
  (cffi:close-foreign-library
   (library-name library)))

(defmethod library-open-p ((library library))
  (cffi:foreign-library-loaded-p library))

(defmacro define-library (name &body initargs)
  `(change-class (cffi::get-foreign-library ',name)
                 'library
                 ,@initargs))
