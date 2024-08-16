(in-package #:org.shirakumo.deploy)

(defvar *foreign-libraries-to-reload* ())
(defvar *compression-factor* T)

(defun deployed-p () NIL)

(defun query-for-library-path ()
  (format *query-io* "~&[DEPLOY] Enter the library path: ")
  (finish-output *query-io*)
  (list (uiop:parse-native-namestring (read-line *query-io*))))

#+(and sbcl win32)
(progn
  (cffi:define-foreign-library libwinpthread
    (:windows "libwinpthread-1.dll"))
  (define-library libwinpthread))

#+(and sbcl nx)
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

#+(and sbcl nx)
(define-hook (:boot stub-compiler) ()
  (sb-ext:with-unlocked-packages ("CL")
    (setf (fdefinition 'cl:compile) #'interpret-compile)
    (setf (fdefinition 'cl:compile-file) (lambda (&rest args) (error "Can't COMPILE-FILE on the NX.")))))

#+sb-core-compression
(handler-case
    (progn
      (sb-ext:assert-version->= 2 2 6)
      (cffi:define-foreign-library compression-lib
        (:windows "libzstd.dll")
        (T (:default "libzstd")))
      (define-library compression-lib))
  (error ()
    ;; Fallback to old
    (cffi:define-foreign-library compression-lib
      (:windows "zlib1.dll")
      (T (:default "libz")))
    (define-library compression-lib)))

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
          (when (or (not (uiop:file-exists-p target))
                    (< (file-write-date target)
                       (file-write-date (library-path lib))))
            (status 1 "Copying library ~a" lib)
            (uiop:copy-file (library-path lib) target))
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
      (setf (slot-value lib 'cffi::pathname) NIL)))
  (setf cffi:*foreign-library-directories* NIL))

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

(define-hook (:build clear-asdf most-negative-fixnum) ()
  (asdf:clear-configuration)
  (setf (fdefinition 'asdf:upgrade-asdf) (lambda ()))
  (dolist (system (asdf:already-loaded-systems))
    (asdf:register-immutable-system system)
    (asdf:clear-system system)))

(defun warmly-boot (system op)
  (let* ((dir (runtime-directory))
         (data (data-directory)))
    #+(and sbcl nx)
    (setf sb-ext:*evaluator-mode* :interpret)
    #+windows
    (unless (uiop:featurep :deploy-console)
      (when (< 0 (cffi:foreign-funcall "AttachConsole" :uint32 #xFFFFFFFF :int))
        #+sbcl
        (flet ((adjust-stream (stream handle i/o)
                 (when (and (/= 0 handle) (/= -1 handle))
                   (setf (symbol-value stream) (sb-sys:make-fd-stream handle i/o T :external-format :utf-16le)))))
          (adjust-stream 'sb-sys:*stdin* (cffi:foreign-funcall "GetStdHandle" :uint32 #xFFFFFFF6 :ssize) :input)
          (adjust-stream 'sb-sys:*stdout* (cffi:foreign-funcall "GetStdHandle" :uint32 #xFFFFFFF5 :ssize) :output)
          (adjust-stream 'sb-sys:*stderr* (cffi:foreign-funcall "GetStdHandle" :uint32 #xFFFFFFF4 :ssize) :output))))
    (if (uiop:featurep :deploy-console)
        (setf *status-output* NIL)
        (setf *status-output* *error-output*))
    (status 0 "Performing warm boot.")
    (when *build-time*
      (multiple-value-bind (s m h dd mm yy) (decode-universal-time *build-time* 0)
        (status 1 "Build time was: ~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d UTC" yy mm dd h m s)))
    (when *source-checksum*
      (status 1 "Source checksum: ~a"
              (with-output-to-string (out)
                (loop for o across *source-checksum*
                      do (format out "~2,'0X" o)))))
    (status 1 "Runtime directory is ~a" dir)
    (status 1 "Resource directory is ~a" data)
    (setf cffi:*foreign-library-directories* (list data dir))
    (status 0 "Running boot hooks.")
    (run-hooks :boot :directory dir :system system :op op)))

(defun quit (&optional system op (exit-code 0))
  (status 0 "Running quit hooks.")
  (handler-bind ((error (lambda (err) (invoke-restart 'report-error err))))
    (run-hooks :quit :system system :op op))
  (uiop:finish-outputs)
  #+sbcl (sb-ext:exit :timeout 1 :code exit-code)
  #-sbcl (uiop:quit exit-code NIL))

(defun call-entry-prepared (entry-point system op)
  ;; We don't handle anything here unless we have no other
  ;; choice, as that should otherwise be up to the user.
  ;; Maybe someone will want a debugger in the end
  ;; application. I can't decide that for them, so we leave
  ;; the possibility open.
  (restart-case
      (handler-bind ((error (lambda (err)
                              (cond ((env-set-p "DEPLOY_DEBUG_BOOT")
                                     (invoke-debugger err))
                                    (T
                                     (status 0 "Encountered unhandled error: ~a" err)
                                     #+nx (uiop:print-backtrace)
                                     (invoke-restart 'exit 1))))))
        (when (env-set-p "DEPLOY_REDIRECT_OUTPUT")
          (redirect-output (uiop:getenv "DEPLOY_REDIRECT_OUTPUT")))
        (warmly-boot system op)
        (status 0 "Launching application.")
        (funcall entry-point)
        (status 0 "Epilogue.")
        (invoke-restart 'exit))
    (exit (&optional (exit-code 0))
      :report "Exit."
      (quit system op exit-code))))

(defclass deploy-op (asdf:program-op)
  ())

(defmethod discover-entry-point ((op deploy-op) (c asdf:system))
  (let ((entry (asdf/system:component-entry-point c)))
    (unless entry
      (error "~a does not specify an entry point." c))
    (or (ignore-errors (uiop:ensure-function entry))
        (let ((class (ignore-errors (uiop:coerce-class entry :error NIL))))
          (when class (lambda () (make-instance class))))
        (error "~a's  entry point ~a is not coercable to a class or function!" c entry))))

;; Do this before to trick ASDF's subsequent usage of UIOP:ENSURE-FUNCTION on the entry-point slot.
(defmethod asdf:perform :before ((o deploy-op) (c asdf:system))
  (let ((entry (discover-entry-point o c)))
    (setf (asdf/system:component-entry-point c)
          (lambda (&rest args)
            (declare (ignore args))
            (call-entry-prepared entry c o)))))

(defmethod asdf:output-files ((o deploy-op) (c asdf:system))
  (values () T))

(defmethod asdf:perform ((o deploy-op) (c asdf:system))
  (status 0 "Running load hooks.")
  (run-hooks :load :system c :op o)
  (status 0 "Gathering system information.")
  (let* ((file #+nx (uiop:getenv "OUTPUT_CORE_PATH")
               #-nx (merge-pathnames (asdf/system:component-build-pathname c)
                                     (asdf:system-relative-pathname c "bin/")))
         (data #+nx (uiop:getenv "DATA_DIRECTORY")
               #-nx (uiop:pathname-directory-pathname file)))
    (setf *foreign-libraries-to-reload* (remove-if-not #'library-open-p
                                                       (remove-if #'library-dont-open-p (list-libraries))))
    (status 1 "Will load the following foreign libs on boot:
      ~s" *foreign-libraries-to-reload*)
    (status 0 "Deploying files to ~a" data)
    (ensure-directories-exist file)
    (ensure-directories-exist data)
    (setf *data-location* (find-relative-path-to data (uiop:pathname-directory-pathname file)))
    (run-hooks :deploy :directory data :system c :op o)
    (status 0 "Running build hooks.")
    (run-hooks :build :system c :op o)
    (status 0 "Dumping image to ~a" file)
    (setf uiop:*image-dumped-p* :executable)
    (setf (fdefinition 'deployed-p) (lambda () T))
    #+windows
    (setf file (make-pathname :type "exe" :defaults file))
    #+(and windows ccl)
    (ccl:save-application file
                          :prepend-kernel T
                          :purify T
                          :toplevel-function #'uiop:restore-image
                          :application-type
                          (if (uiop:featurep :deploy-console)
                              :console
                              :gui))
    
    #+(and sbcl nx)
    (sb-ext:save-lisp-and-die file :toplevel #'uiop:restore-image)
    #-(or (and windows ccl) (and sbcl nx))
    (apply #'uiop:dump-image file
           (append '(:executable T)
                   #+sb-core-compression
                   `(:compression ,*compression-factor*)
                   #+(and sbcl os-windows)
                   `(:application-type
                     ,(if (uiop:featurep :deploy-console)
                          :console
                          :gui))))))

;; hook ASDF
(flet ((export! (symbol package)
         (import symbol package)
         (export symbol package)))
  (export! 'deploy-op :asdf/bundle)
  (export! 'deploy-op :asdf))
