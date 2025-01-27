(in-package #:org.shirakumo.deploy)

(defvar *compression-factor* T)

(defun deployed-p () NIL)

#+(and sbcl win32)
(progn
  (cffi:define-foreign-library libwinpthread
    (:windows "libwinpthread-1.dll"))
  (define-library libwinpthread))

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

(defun call-entry-prepared (entry-point &rest args)
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
                                     #+(and nx asdf3) (uiop:print-backtrace)
                                     (invoke-restart 'exit 1))))))
        (when (env-set-p "DEPLOY_REDIRECT_OUTPUT")
          (redirect-output (envvar "DEPLOY_REDIRECT_OUTPUT")))
        (apply #'warmly-boot args)
        (status 0 "Launching application.")
        (funcall entry-point)
        (status 0 "Epilogue.")
        (invoke-restart 'exit))
    (exit (&optional (exit-code 0))
      :report "Exit."
      (apply #'quit exit-code args))))

(defun warmly-boot (&rest args)
  #+windows
  (unless (featurep :deploy-console)
    (when (< 0 (cffi:foreign-funcall "AttachConsole" :uint32 #xFFFFFFFF :int))
      #+sbcl
      (flet ((adjust-stream (stream handle i/o)
               (when (and (/= 0 handle) (/= -1 handle))
                 (setf (symbol-value stream) (sb-sys:make-fd-stream handle i/o T :external-format :utf-16le)))))
        (adjust-stream 'sb-sys:*stdin* (cffi:foreign-funcall "GetStdHandle" :uint32 #xFFFFFFF6 :ssize) :input)
        (adjust-stream 'sb-sys:*stdout* (cffi:foreign-funcall "GetStdHandle" :uint32 #xFFFFFFF5 :ssize) :output)
        (adjust-stream 'sb-sys:*stderr* (cffi:foreign-funcall "GetStdHandle" :uint32 #xFFFFFFF4 :ssize) :output))))
  (if (featurep :deploy-console)
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
  (let* ((dir (runtime-directory))
         (data (data-directory)))
    (status 1 "Runtime directory is ~a" dir)
    (status 1 "Resource directory is ~a" data)
    #+cffi (setf cffi:*foreign-library-directories* (list data dir))
    (status 0 "Running boot hooks.")
    (apply #'run-hooks :boot :directory dir args)))

(defun quit (&optional (exit-code 0) &rest args)
  (status 0 "Running quit hooks.")
  (handler-bind ((error (lambda (err) (invoke-restart 'report-error err))))
    (apply #'run-hooks :quit args))
  (finish-output *standard-output*)
  (finish-output *error-output*)
  (finish-output *trace-output*)
  #+(and sbcl (not nx)) (sb-ext:exit :timeout 1 :code exit-code)
  #+(and asdf3 (not sbcl) (not nx)) (uiop:quit exit-code NIL)
  #+nx (cffi:foreign-funcall "nx_exit" :int exit-code))

(defclass deploy-op (#+asdf3 asdf:program-op)
  ((entry-point :initarg :entry-point :initform NIL :accessor entry-point)
   (output-file :initarg :output-file :initform NIL :accessor output-file)))

(defmethod output-file ((op deploy-op))
  (or (slot-value op 'output-file)
      (let ((file #+nx (envvar "OUTPUT_CORE_PATH")
                  #-nx (merge-pathnames "bin/application" *here*)))
        (cond ((featurep :deploy-image)
               (make-pathname :type "core" :defaults file))
              ((featurep :windows)
               (make-pathname :type "exe" :defaults file))
              (T
               file)))))

(defmethod deploy ((op symbol) &rest args &key type &allow-other-keys)
  (remf args :type)
  (deploy (apply #'make-instance op args) :type type))

(defmethod deploy :before ((op deploy-op) &key &allow-other-keys)
  (status 0 "Running load hooks.")
  (run-hooks :load :op op)
  (status 0 "Gathering system information.")
  (setf *foreign-libraries-to-reload* (remove-if-not #'library-open-p
                                                     (remove-if #'library-dont-open-p (list-libraries))))
  (status 1 "Will load the following foreign libs on boot:
      ~s" *foreign-libraries-to-reload*)
  (let* ((file (output-file op))
         (data #+nx (envvar "DATA_DIRECTORY")
               #-nx (pathname-utils:to-directory file)))
    (status 0 "Deploying files to ~a" data)
    (ensure-directories-exist file)
    (ensure-directories-exist data)
    #-nx
    (setf *data-location* (make-pathname :host NIL :device NIL :defaults
                                         (pathname-utils:relative-pathname
                                          (pathname-utils:to-directory file) data)))
    (run-hooks :deploy :directory data :op op)
    (status 0 "Running build hooks.")
    (run-hooks :build :op op)
    (status 0 "Dumping image to ~a" file)
    (setf (fdefinition 'deployed-p) (lambda () T))))

(defmethod deploy ((op deploy-op) &key type entry-point)
  (unless entry-point (setf entry-point (entry-point op)))
  (deploy (output-file op)
          :type (or type 
                    (cond ((featurep :deploy-console) :console)
                          ((featurep :deploy-image) :image)
                          (T :executable)))
          :entry-point (when entry-point
                         (lambda ()
                           (call-entry-prepared entry-point)))))

(defclass deploy-console-op (deploy-op) ())

(defmethod deploy ((op deploy-console-op) &rest args &key &allow-other-keys)
  (pushnew :deploy-console *features*)
  (apply #'call-next-method op :type :console args))

(defclass deploy-image-op (deploy-op)
  ((entry-point :initform NIL)))

(defmethod output-file ((op deploy-image-op))
  (make-pathname :type "core" :defaults (call-next-method)))

(defmethod deploy ((op deploy-image-op) &rest args &key &allow-other-keys)
  (apply #'call-next-method op :type :image args))

(defmethod deploy ((file pathname) &key (type :executable) entry-point)
  ;; Have to do this crap ourselves since UIOP sucks
  #+ccl
  (apply #'ccl:save-application file
         :prepend-kernel (not (eql type :image))
         :purify T
         :toplevel-function entry-point
         #+windows
         (ecase type
           (:console '(:application-type :console))
           (:executable '(:application-type :gui))
           (:image ()))
         ())
  
  #+sbcl
  (apply #'sb-ext:save-lisp-and-die file
         :toplevel entry-point
         (ecase type
           ((:executable :console)
            `(:executable T
                          #+(and sbcl windows) ,@(list :application-type (if (eql type :executable) :gui :console))
                          #+sb-core-compression ,@(list :compression *compression-factor*)))
           (:image
            `(:executable NIL))))
  #+(and asdf3 (not (or ccl sbcl)))
  (progn (setf uiop:*image-entry-point* entry-point)
         (apply #'uiop:deploy-image file
                (ecase type
                  ((:executable :console)
                   `(:executable T))
                  (:image
                   `(:executable NIL))))))
