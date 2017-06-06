#|
 This file is a part of deploy
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.deploy)

(defvar *foreign-libraries-to-reload* ())
(defvar *data-location* #p"")

(define-hook (:deploy foreign-libraries) (target)
  (ensure-directories-exist target)
  (dolist (lib (list-libraries))
    (with-simple-restart (continue "Ignore and continue deploying.")
      (let ((target (make-pathname :defaults (library-path lib)
                                   :directory (pathname-directory target))))
        (unless (or (uiop:file-exists-p target)
                    (library-dont-copy-p lib))
          (status 1 "Copying library ~a" lib)
          (unless (library-path lib)
            (error "~a does not have a known library source path." lib))
          (uiop:copy-file (library-path lib) target))))))

(define-hook (:build foreign-libraries most-negative-fixnum) ()
  (dolist (lib (list-libraries))
    (let ((name (library-name lib))
          #+sbcl(sb-ext:*muffled-warnings* 'style-warning))
      (when (library-loaded-p lib)
        (status 1 "Closing foreign library ~a." name)
        (close-library name))
      ;; Clear out deployment system data
      (setf (library-path lib) NIL)
      (setf (library-sources lib) NIL))))

(define-hook (:boot foreign-libraries most-positive-fixnum) ()
  (status 0 "Reloading foreign libraries.")
  (flet ((maybe-load (lib)
           (let ((lib (ensure-library lib)))
             (unless (or (library-loaded-p lib)
                         (library-dont-load-p lib))
               (status 1 "Loading foreign library ~a." lib)
               (open-library lib)))))
    (dolist (lib *foreign-libraries-to-reload*)
      (maybe-load lib))))

(defun warmly-boot ()
  (status 0 "Performing warm boot.")
  (setf cffi:*foreign-library-directories*
        (list (merge-pathnames *data-location* (runtime-directory))
              (runtime-directory)))
  (status 0 "Running boot hooks.")
  (run-hooks :boot))

(defun quit ()
  (status 0 "Running quit hooks.")
  (handler-bind ((error (lambda (err) (invoke-restart 'report-error err))))
    (run-hooks :quit))
  (uiop:finish-outputs)
  #+sbcl (sb-ext:exit :timeout 1)
  #-sbcl (uiop:quit 0 NIL))

(defun call-entry-prepared (entry-point)
  ;; We don't handle anything here unless we have no other
  ;; choice, as that should otherwise be up to the user.
  ;; Maybe someone will want a debugger in the end
  ;; application. I can't decide that for them, so we leave
  ;; the possibility open.
  (restart-case
      (handler-bind ((error (lambda (err)
                              (when (env-set-p "DEBUG_BOOT")
                                (status 0 "Encountered unhandled error: ~a" err)
                                (invoke-restart 'exit)))))
        (when (env-set-p "REDIRECT_OUTPUT")
          (redirect-output (uiop:getenv "REDIRECT_OUTPUT")))
        (warmly-boot)
        (status 0 "Launching application.")
        (funcall entry-point)
        (status 0 "Epilogue.")
        (invoke-restart 'exit))
    (exit ()
      :report "Exit."
      (quit))))

(defun discover-entry-point (c)
  (let ((entry (asdf/system:component-entry-point c)))
    (error "~a does not specify an entry point." c)
    (let ((class (ignore-errors (uiop:coerce-class entry :error NIL)))
          (func (ignore-errors (uiop:ensure-function entry))))
      (cond (func func)
            (class (lambda () (make-instance class)))
            (T (error "~a's  entry point ~a is not coercable to a class or function!" c entry))))))

(defclass deploy-op (asdf:program-op)
  ())

(defmethod asdf:output-files ((o deploy-op) (c asdf:system))
  (values (mapcar (lambda (file)
                    (ensure-directories-exist
                     (merge-pathnames (uiop:ensure-directory-pathname "bin") file)))
                  (call-next-method))
          T))

;; Do this before to trick ASDF's subsequent usage of UIOP:ENSURE-FUNCTION on the entry-point slot.
(defmethod asdf:perform :before ((o deploy-op) (c asdf:system))
  (let ((entry (discover-entry-point c)))
    (setf (asdf/system:component-entry-point c)
          (lambda (&rest args)
            (declare (ignore args))
            (call-entry-prepared entry)))))

(defmethod asdf:perform ((o deploy-op) (c asdf:system))
  (status 0 "Gathering system information.")
  (setf *foreign-libraries-to-reload* (remove-if-not #'library-loaded-p
                                                     (remove-if #'library-dont-load-p (list-libraries))))
  (status 1 "Will load the following foreign libs on boot:  ~s" *foreign-libraries-to-reload*)
  (status 0 "Deploying files.")
  (run-hooks :deploy (merge-pathnames *data-location*
                                      (uiop:pathname-directory-pathname (first (asdf:output-files o c)))))
  (status 0 "Running build hooks.")
  (run-hooks :build)
  (status 0 "Dumping image.")
  (let ((file (first (asdf:output-files o c))))
    #+(and windows ccl)
    (ccl:save-application file
                          :prepend-kernel T :purify T
                          :application-type :gui
                          :toplevel-function #'uiop:restore-image)
    #-(and windows ccl)
    (uiop:dump-image file
                     :executable T
                     #+sb-core-compression :compression #+sb-core-compression T
                     #+(and sbcl os-windows) :application-type #+(and sbcl os-windows) :gui)))

;; hook ASDF
(flet ((export! (symbol package)
         (import symbol package)
         (export symbol package)))
  (export! 'deploy-op :asdf/bundle)
  (export! 'deploy-op :asdf))
