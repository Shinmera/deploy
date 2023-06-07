#|
 This file is a part of deploy
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.deploy)

(defparameter *system-source-directories*
  (list #+windows (or #+(or allegro clisp clozure cmucl gcl lispworks sbcl scl xcl)
                      (first (uiop:raw-command-line-arguments))
                      #+(or clasp ecl) (si:argv 0) #+mkcl (mkcl:argv 0))
        #+windows #p"C:/Windows/system32/"
        #+(and windows x86) #p"C:/Windows/SysWoW64/"
        #+windows #p"C:/Windows/"
        #+windows #p"C:/Windows/System32/Wbem"
        #+unix #p"/usr/lib/"
        #+unix #p"/usr/local/lib/"
        #+(and unix x86-64) #p"/usr/lib64/"
        #+(and unix x86-64) #p"/usr/lib/x86_64-linux-gnu/"
        #+(and unix x86) #p"/usr/lib/x86-linux-gnu/"
        #+(and unix arm64) #p"/usr/lib/aarch64-linux-gnu/"
        #+(and unix arm) #p"/usr/lib/arm-linux-gnueabi/"
        #+(and unix arm) #p"/usr/lib/arm-linux-gnueabihf/"
        #+unix #p"/usr/lib/*/"
        #+darwin #p"/opt/local/lib"
        #+darwin #p"/usr/local/Cellar/**/lib/"))

(defun list-libraries ()
  (mapcar #'ensure-library (cffi:list-foreign-libraries :loaded-only NIL)))

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
  (append (when (cffi:foreign-library-pathname library)
            (list (cffi:foreign-library-pathname library)))
          (resolve-cffi-spec (slot-value library 'cffi::spec))
          (list (make-lib-pathname (format NIL "*~(~a~)*" (library-name library))))))

(defmethod possible-pathnames (library)
  (possible-pathnames (ensure-library library)))

(defmethod possible-directories ((library library))
  ;; FIXME: Maybe use ld.so.cache
  (remove NIL
          (append (library-sources library)
                  #-ccl (list (cffi::foreign-library-handle library))
                  #+ccl (let ((handle (cffi::foreign-library-handle library)))
                          (when handle
                            (list (ccl::shlib.pathname handle))))
                  (cffi::foreign-library-search-path library)
                  (when (library-system library)
                    (discover-subdirectories
                     (asdf:system-source-directory
                      (library-system library))))
                  (loop for form in cffi:*foreign-library-directories*
                        for result = (eval form)
                        append (if (listp result) result (list result)))
                  *system-source-directories*
                  #+windows (env-paths "PATH")
                  #+(and unix (not darwin)) (env-paths "LD_LIBRARY_PATH")
                  #+darwin (env-paths "DYLD_LIBRARY_PATH"))))

(defun elf-file-p (path)
  (with-open-file (elf path :element-type '(unsigned-byte 8) :if-does-not-exist :error)
    ;; All ELF files begin with the same four bytes
    (and (= (read-byte elf) #x7f)
         (= (read-byte elf) #x45)
         (= (read-byte elf) #x4c)
         (= (read-byte elf) #x46))))

(defun follow-ld-script (path)
  (if (elf-file-p path)
      path
      (let* ((file-lines (uiop:read-file-lines path))
             (group-line (first (remove-if-not (lambda (line)
                                                 (uiop:string-prefix-p "GROUP ( " line))
                                               file-lines)))
             (path-to-lib (fourth (uiop:split-string group-line :separator "( )"))))
        path-to-lib)))

(defun ensure-shared-library-file (path)
  "Some linux distributions keep ld scripts in the lib directories as links, follow them if necessary"
  #+linux (follow-ld-script path)
  #-linux path)

(defmethod possible-directories (library)
  (possible-directories (ensure-library library)))

(defmethod find-source-file ((library library))
  (let ((sources (possible-directories library)))
    (dolist (path (possible-pathnames library))
      (when path
        (loop with filename = (pathname-filename path)
              for source in sources
              for files = (directory (merge-pathnames filename source))
              do (when files
                   (return-from find-source-file (first (mapcar #'ensure-shared-library-file files)))))))))

(defmethod find-source-file (library)
  (find-source-file (ensure-library library)))

(defmethod shared-initialize :after ((library library) slots &key)
  (unless (library-path library)
    (setf (library-path library) (find-source-file library))))

(defmethod library-name ((library library))
  (cffi:foreign-library-name library))

(defmethod library-name (library)
  (library-name (ensure-library library)))

(defmethod open-library ((library library))
  (cffi:load-foreign-library (library-name library)))

(defmethod open-library (library)
  (open-library (ensure-library library)))

(defmethod close-library ((library library))
  (cffi:close-foreign-library
   (library-name library)))

(defmethod close-library (library)
  (close-library (ensure-library library)))

(defmethod library-open-p ((library library))
  (cffi:foreign-library-loaded-p library))

(defmethod library-open-p (library)
  (library-open-p (ensure-library library)))

(defmacro define-library (name &body initargs)
  `(change-class (cffi::get-foreign-library ',name)
                 'library
                 ,@initargs))
