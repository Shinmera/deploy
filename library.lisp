(in-package #:org.shirakumo.deploy)

(defvar *foreign-libraries-to-reload* ())
(defparameter *system-source-directories*
  (list #+windows (first (command-line-arguments))
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
        #+darwin #p"/usr/local/Cellar/**/lib/"
        #+nx (merge-pathnames "nro/" (envvar "DATA_DIRECTORY"))))

(defun list-libraries ()
  (mapcar #'ensure-library
          #+cffi (cffi:list-foreign-libraries :loaded-only NIL)
          #-cffi ()))

(defun ensure-library (library)
  (etypecase library
    (library library)
    #+cffi (cffi:foreign-library (change-class library 'library))
    (symbol #+cffi (ensure-library (cffi::get-foreign-library library)))))

(defclass library (#+cffi cffi:foreign-library)
  ((sources :initarg :sources :initform () :accessor library-sources)
   (path :initarg :path :initform NIL :accessor library-path)
   (dont-open :initarg :dont-open :initform NIL :accessor library-dont-open-p)
   (dont-deploy :initarg :dont-deploy :initform NIL :accessor library-dont-deploy-p)))

(defmethod print-object ((library library) stream)
  (print-unreadable-object (library stream :type T)
    (format stream "~a" (library-name library))))

(defmethod possible-pathnames ((library library))
  (let ((paths (list (make-lib-pathname (format NIL "*~(~a~)*" (library-name library))))))
    #+cffi
    (when (cffi:foreign-library-pathname library)
      (push (cffi:foreign-library-pathname library) paths))
    (append #+cffi (resolve-cffi-spec (slot-value library 'cffi::spec))
            (nreverse paths))))

(defmethod possible-pathnames (library)
  (possible-pathnames (ensure-library library)))

(defmethod possible-directories ((library library))
  ;; FIXME: Maybe use ld.so.cache
  (remove NIL
          (append (library-sources library)
                  #-ccl (list #+cffi (cffi::foreign-library-handle library))
                  #+ccl (let ((handle (cffi::foreign-library-handle library)))
                          (when handle
                            (list (ccl::shlib.pathname handle))))
                  #+cffi (cffi::foreign-library-search-path library)
                  #+cffi
                  (loop for form in cffi:*foreign-library-directories*
                        for result = (eval form)
                        append (if (listp result) result (list result)))
                  *system-source-directories*
                  #+windows (envvar-directories "PATH")
                  #+(and unix (not darwin)) (envvar-directories "LD_LIBRARY_PATH")
                  #+darwin (envvar-directories "DYLD_LIBRARY_PATH"))))

(defun elf-file-p (path)
  (ignore-errors
   (with-open-file (elf path :element-type '(unsigned-byte 8) :if-does-not-exist :error)
     ;; All ELF files begin with the same four bytes
     (and (= (read-byte elf) #x7f)
          (= (read-byte elf) #x45)
          (= (read-byte elf) #x4c)
          (= (read-byte elf) #x46)))))

(defun follow-ld-script (path)
  (if (elf-file-p path)
      path
      (with-open-file (stream path :if-does-not-exist NIL)
        (when stream
          (loop for line = (read-line stream NIL NIL)
                while line
                do (when (string= "GROUP (" line :end2 (length "GROUP ("))
                     (return (first (split #\  line :start "GROUP (" :end (position #\) line))))))))))

(defun ensure-shared-library-file (path)
  "Some linux distributions keep ld scripts in the lib directories as links, follow them if necessary"
  #+linux (follow-ld-script path)
  #-linux path)

(defmethod possible-directories (library)
  (possible-directories (ensure-library library)))

(defmethod find-source-file ((library library))
  (let ((sources (possible-directories library)))
    (dolist (path (possible-pathnames library))
      (loop with filename = (pathname-filename path)
            for source in sources
            for files = (directory (merge-pathnames filename source))
            do (when files
                 (return-from find-source-file (first (mapcar #'ensure-shared-library-file files))))))))

(defmethod find-source-file (library)
  (find-source-file (ensure-library library)))

(defmethod shared-initialize :after ((library library) slots &key)
  (unless (library-path library)
    (setf (library-path library) (find-source-file library))))

(defmethod library-name ((library library))
  #+cffi (cffi:foreign-library-name library))

(defmethod library-name (library)
  (library-name (ensure-library library)))

(defmethod library-soname ((library library))
  (library-soname (library-path library)))

(defmethod library-soname (library)
  (library-soname (ensure-library library)))

(defmethod library-dependencies ((library library))
  (library-dependencies (library-path library)))

(defmethod library-dependencies (library)
  (library-dependencies (ensure-library library)))

(defmethod open-library ((library library))
  #+cffi (cffi:load-foreign-library (library-name library)))

(defmethod open-library (library)
  (open-library (ensure-library library)))

(defmethod close-library ((library library))
  #+cffi (cffi:close-foreign-library (library-name library)))

(defmethod close-library (library)
  (close-library (ensure-library library)))

(defmethod library-open-p ((library library))
  #+cffi (cffi:foreign-library-loaded-p library))

(defmethod library-open-p (library)
  (library-open-p (ensure-library library)))

(defmacro define-library (name &body initargs)
  #+cffi
  `(change-class (cffi::get-foreign-library ',name)
                 'library
                 ,@initargs)
  #-cffi
  `(make-instance 'library ,@initargs))

(defmethod patch-soname ((library library))
  (patch-soname (library-path library)))

(defmethod patch-soname (library)
  (patch-soname (ensure-library library)))

(defmethod patch-dependencies ((library library) changes)
  (patch-dependencies (library-path library) changes))

(defmethod patch-dependencies (library changes)
  (patch-dependencies (ensure-library library) changes))

(defmethod patch-dependencies ((path pathname) (changes (eql T)))
  (patch-dependencies
   path
   (let* ((locals #-(or darwin windows) (remove-if-not #'elf-file-p (directory-contents path))
                  #+darwin (directory-contents path :type "dylib")
                  #+windows (directory-contents path :type "dll"))
          (locals (loop for local in locals
                        for soname = (library-soname local)
                        when soname collect (list soname local))))
     (loop for dependency in (library-dependencies path)
           for relative = (loop for (soname local) in locals
                                when (string= soname dependency)
                                do (return (file-namestring local)))
           when (and relative (string/= dependency relative))
           collect (list dependency relative)))))

(defmethod library-soname ((path pathname))
  (or #+(and asdf3 linux)
      (ignore-errors
       (let ((out (uiop:run-program (list "patchelf" "--print-soname" (pathname-utils:native-namestring path)) :output :string)))
         (string-right-trim '(#\Linefeed) out)))
      #+(and asdf3 darwin)
      (ignore-errors
       (let ((out (uiop:run-program (list "otool" "-D" (pathname-utils:native-namestring path)) :output :string)))
         (subseq out (or (position #\/ out :from-end T)
                         (position #\Space out :from-end T)
                         (position #\Linefeed out :from-end T)))))
      (pathname-name path)))

(defmethod library-dependencies ((path pathname))
  #+(and asdf3 linux)
  (split #\Linefeed (uiop:run-program (list "patchelf" "--print-needed" (pathname-utils:native-namestring path)) :output :string))
  #+(and asdf3 darwin)
  (let ((out (split #\Linefeed (uiop:run-program (list "otool" "-L" (pathname-utils:native-namestring path)) :output :string))))
    (loop for line in (cddr out) ; First two lines are the file itself again.
          for trimmed = (string-trim '(#\Space #\Tab #\Linefeed #\Return) line)
          for space = (position #\Space trimmed)
          collect (if space (subseq trimmed 0 space) trimmed))))

(defmethod patch-soname ((path pathname))
  (let ((name (pathname-filename path)))
    #+(and asdf3 linux)
    (uiop:run-program (list "patchelf" "--set-soname" name (pathname-utils:native-namestring path)))
    #+(and asdf3 darwin)
    (uiop:run-program (list "install_name_tool" "-id" name (pathname-utils:native-namestring path)))))

(defmethod patch-dependencies ((path pathname) (changes list))
  (when changes
    (status 2 "Patching dependencies of ~a:~{~%  ~{~a => ~a~}~}" path changes)
    #+(and asdf3 linux)
    (uiop:run-program (append (list "patchelf")
                              (loop for (src dst) in changes
                                    collect "--replace-needed"
                                    collect src collect dst)
                              (list (pathname-utils:native-namestring path))))
    #+(and asdf3 darwin)
    (uiop:run-program (append (list "install_name_tool")
                              (loop for (src dst) in changes
                                    collect "-change"
                                    collect src collect dst)
                              (list (pathname-utils:native-namestring path))))))

#+cffi
(define-hook (:deploy foreign-libraries) (directory)
  #+nx (setf directory (merge-pathnames "nro/" directory))
  (ensure-directories-exist directory)
  (dolist (lib #+sb-core-compression (list* (ensure-library 'compression-lib) (list-libraries))
               #-sb-core-compression (list-libraries))
    (with-simple-restart (continue "Ignore and continue deploying.")
      (unless (library-dont-deploy-p lib)
        (unless (library-path lib)
          #-nx
          (restart-case (error "~a does not have a known shared library file path." lib)
            (provide-path (path)
              :report "Provide the path to the library manually."
              :interactive query-for-library-path
              (setf (library-path lib) path)))
          #+nx
          (progn (warn "~a does not have a known shared library file path." lib)
                 (continue)))
        (let ((target (make-pathname :directory (pathname-directory directory)
                                     :device (pathname-device directory)
                                     :host (pathname-host directory)
                                     :defaults (library-path lib))))
          (when (or (not (probe-file target))
                    (< (file-write-date target)
                       (file-write-date (library-path lib))))
            (status 1 "Copying library ~a" lib)
            (copy-file (library-path lib) target))
          ;; Force the library spec
          (setf (slot-value lib 'cffi::spec) `((T ,(file-namestring target)))))))))

(define-hook (:build foreign-libraries (+ most-negative-fixnum 10)) ()
  (dolist (lib (list-libraries))
    (let (#+sbcl(sb-ext:*muffled-warnings* 'style-warning))
      (when (library-open-p lib)
        (status 1 "Closing foreign library ~a." lib)
        (close-library lib))
      ;; Clear out deployment system data
      (setf (library-path lib) NIL)
      (setf (library-sources lib) NIL)
      #+cffi (setf (slot-value lib 'cffi::pathname) NIL)))
  #+cffi (setf cffi:*foreign-library-directories* NIL))

(define-hook (:boot foreign-libraries (- most-positive-fixnum 10)) ()
  (status 0 "Reloading foreign libraries.")
  (flet ((maybe-load (lib)
           (let ((lib (ensure-library lib))
                 #+sbcl(sb-ext:*muffled-warnings* 'style-warning))
             (unless (or (library-open-p lib)
                         (library-dont-open-p lib))
               (status 1 "Loading foreign library ~a." lib)
               (open-library lib)))))
    (dolist (lib *foreign-libraries-to-reload*)
      (maybe-load lib))))
