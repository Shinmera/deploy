(in-package #:org.shirakumo.deploy)

(defvar *sbcl-source-tree*
  (parent-directory (parent-directory (second (first (logical-pathname-translations "SYS"))))))

(defun sbcl-path (path)
  (merge-pathnames path *sbcl-source-tree*))

(defun cvar (id alist)
  (cdr (assoc id alist :test #'string-equal)))

(defun run (program &rest args)
  (flet ((normalize (arg)
           (etypecase arg
             (pathname (sb-ext:native-namestring arg))
             (real (princ-to-string arg))
             (string arg))))
    (sb-ext:run-program program
                        (loop for arg in args
                              append (mapcar #'normalize (if (listp arg) arg (list arg))))
                        :search T :output *error-output*)))

(defclass create-core-op (asdf:operation) ())

(defmethod asdf:input-files ((o create-core-op) (c asdf:system))
  (values (list (sbcl-path "run-sbcl.sh"))
          T))

(defmethod asdf:output-files ((o create-core-op) (c asdf:system))
  (values (list (asdf:system-relative-pathname c (asdf:component-name c) :type "core"))
          T))

(defmethod asdf:perform ((o create-core-op) (c asdf:system))
  (destructuring-bind (run-sbcl) (asdf:input-files o c)
    (destructuring-bind (core) (asdf:output-files o c)
      (run run-sbcl
           "--eval" (format NIL "~s" `(asdf:load-system ,(asdf:component-name c)))
           "--eval" (format NIL "~s" `(sb-ext:save-lisp-and-die ,core))))))

(defclass shrinkwrap-op (asdf:program-op)
  ((asdf:selfward-operation :initform '(create-core-op) :allocation :class)))

(defmethod asdf:input-files ((o shrinkwrap-op) (c asdf:system))
  (values (list (asdf:system-relative-pathname c (asdf:component-name c) :type "core")
                (sbcl-path "tools-for-build/elftool.lisp")
                (sbcl-path "run-sbcl.sh")
                (sbcl-path "src/runtime/sbcl.mk"))
          T))

(defmethod asdf:output-files ((o shrinkwrap-op) (c asdf:system))
  (values (list (asdf:system-relative-pathname c (asdf:component-name c) :type "s")
                (asdf:system-relative-pathname c (asdf:component-name c) :type "o")
                (asdf:system-relative-pathname c (asdf:component-name c)))
          T))

(defmethod asdf:perform ((o shrinkwrap-op) (c asdf:system))
  (destructuring-bind (core elftool run-sbcl cvars) (asdf:input-files o c)
    (destructuring-bind (symbol object bin) (asdf:output-files o c)
      (setf cvars (parse-vars cvars))
      (run run-sbcl "--script" elftool "split" core symbol)
      (run (cvar "CC" cvars)
           "-no-pie" (cvar "LINKFLAGS" cvars)  (cvar "CFLAGS" cvars)
           "-o" bin symbol object (cvar "LIBS" cvars)))))
