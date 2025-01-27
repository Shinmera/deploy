(in-package #:org.shirakumo.deploy)

(define-hook (:build clear-asdf most-negative-fixnum) ()
  (asdf:clear-configuration)
  (setf (fdefinition 'asdf:upgrade-asdf) (lambda ()))
  #+quicklisp (setf ql:*local-project-directories* ())
  (dolist (system (asdf:already-loaded-systems))
    (asdf:register-immutable-system system)
    (asdf:clear-system system)))

(define-hook (:build uiop) ()
  (unless uiop/image:*image-dumped-p*
    (setf uiop/image:*image-dumped-p* T)
    (setf uiop/image::*image-restored-p* :in-regress)
    (uiop/image:call-image-dump-hook)
    (setf uiop/image::*image-restored-p* NIL)))

(define-hook (:boot uiop) ()
  (unless uiop/image::*image-restored-p*
    (setf uiop/image::*image-restored-p* :in-progress)
    (uiop/image:call-image-restore-hook)
    (setf uiop/image::*image-restored-p* T)))

;; KLUDGE: Apparently MAKE-PLAN is NOT called on DEPLOY-OP first. Fun.
(defmethod asdf/plan:make-plan :before (plan (o asdf/operate:build-op) (c asdf:system) &key)
  (run-hooks :pre-load :system c :op o))

(defmethod discover-entry-point ((op deploy-op) (c asdf:system))
  (or (entry-point op)
      (setf (entry-point op)
            (let ((entry (asdf/system:component-entry-point c)))
              (unless entry
                (error "~a does not specify an entry point." c))
              (or (ignore-errors (uiop:ensure-function entry))
                  (let ((class (ignore-errors (uiop:coerce-class entry :error NIL))))
                    (when class (lambda () (make-instance class))))
                  (error "~a's  entry point ~a is not coercable to a class or function!" c entry))))))

(defmethod asdf:output-files ((o deploy-op) (c asdf:system))
  (let ((default (let ((file #+nx (getenv "OUTPUT_CORE_PATH")
                             #-nx (merge-pathnames (asdf/system:component-build-pathname c)
                                                   (asdf:system-relative-pathname c "bin/"))))
                   (cond ((featurep :deploy-image)
                          (make-pathname :type "core" :defaults file))
                         ((featurep :windows)
                          (make-pathname :type "exe" :defaults file))
                         (T
                          file)))))
    (values (list (or (slot-value o 'output-file)
                      (setf (output-file o)  default)))
            T)))

(defmethod asdf:perform :before ((o deploy-op) (c asdf:system))
  ;; Do this before to trick ASDF's subsequent usage of UIOP:ENSURE-FUNCTION on the entry-point slot.
  (let ((entry (discover-entry-point o c)))
    (setf (asdf/system:component-entry-point c)
          (lambda (&rest args)
            (declare (ignore args))
            (call-entry-prepared entry c o)))))

(defmethod asdf:perform ((o deploy-op) (c asdf:system))
  (deploy o))

(defmethod deploy ((o symbol) &rest args &key type system &allow-other-keys)
  (remf args :type)
  (remf args :system)
  (if system
      (asdf:oos (apply #'make-instance o args) system)
      (deploy (apply #'make-instance o args) :type type)))

;; hook ASDF
(flet ((export! (symbol package)
         (import symbol package)
         (export symbol package)))
  (export! 'deploy-op :asdf/bundle)
  (export! 'deploy-op :asdf)
  (export! 'deploy-image-op :asdf/bundle)
  (export! 'deploy-image-op :asdf)
  (export! 'deploy-console-op :asdf/bundle)
  (export! 'deploy-console-op :asdf))
