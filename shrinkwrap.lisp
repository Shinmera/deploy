(in-package #:org.shirakumo.deploy)

(defvar *sbcl-source-tree*
  (ignore-errors
   (pathname-utils:to-directory
    (pathname-utils:pop-directory (second (first (logical-pathname-translations "SYS"))) 2))))

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
    (let ((args (loop for arg in args
                      append (mapcar #'normalize (if (listp arg) arg (list arg))))))
      (status 2 "Running ~a ~@<~@;~{~%~a~}~;~:>~%" program args)
      (unless (= 0 (sb-ext:process-exit-code
                    (sb-ext:run-program program args :search T :output *error-output*)))
        (error "Failed to run~%  ~a~{~%   ~a~}" program args)))))

(defclass create-core-op (asdf:monolithic-bundle-op) ())

(defmethod asdf:component-depends-on ((o create-core-op) (c asdf:system))
  (list (list 'asdf:load-op c)))

(defmethod asdf:input-files ((o create-core-op) (c asdf:system))
  (values (list (sbcl-path "run-sbcl.sh"))
          T))

(defmethod asdf:output-files ((o create-core-op) (c asdf:system))
  (asdf:output-files 'deploy-image-op c))

(defmethod asdf:perform ((o create-core-op) (c asdf:system))
  (destructuring-bind (run-sbcl) (asdf:input-files o c)
    (with-standard-io-syntax
      (run run-sbcl
           "--lose-on-corruption" "--disable-ldb" "--disable-debugger"
           "--eval" "(asdf:load-system :deploy :verbose NIL)"
           "--eval" "(sb-ext:without-package-locks
                       (setf deploy:*compression-factor* NIL)
                       (set (read-from-string \"sb-alien::*extern-alien-dynamic-lookup-p*\") T))"
           "--eval" (format NIL "(asdf:oos 'deploy:deploy-image-op ~s)" (asdf:component-name c))))))

(defclass shrinkwrap-op (asdf:monolithic-bundle-op) ())

(defmethod asdf:component-depends-on ((o shrinkwrap-op) (c asdf:system))
  (list (list 'create-core-op c)))

(defmethod asdf:input-files ((o shrinkwrap-op) (c asdf:system))
  (values (list (asdf:output-file 'create-core-op c)
                (sbcl-path "tools-for-build/elftool.lisp")
                (sbcl-path "run-sbcl.sh")
                (sbcl-path "src/runtime/sbcl.mk"))
          T))

(defmethod asdf:output-files ((o shrinkwrap-op) (c asdf:system))
  (values (list (funcall asdf::*output-translation-function*
                         (asdf:system-relative-pathname c (asdf:component-name c) :type "s"))
                (funcall asdf::*output-translation-function*
                         (asdf:system-relative-pathname c (format NIL "~a-core" (asdf:component-name c)) :type "o"))
                (asdf:output-file 'deploy-op c))
          T))

(defmethod link-libraries ((o shrinkwrap-op) cvars)
  (delete-duplicates
   (append (remove "-lzstd" (cvar "LIBS" cvars) :test #'string=)
           (loop for lib in (list-libraries)
                 collect (format NIL "-L~a" (make-pathname :name NIL :type NIL :defaults (library-path lib)))
                 collect (format NIL "-l:~a" (pathname-filename (library-path lib)))))
   :test #'string=))

(defun sbcl-objs ()
  (let* ((objs (uiop:run-program (list "make" "-C" (uiop:native-namestring (sbcl-path "src/runtime/"))
                                       "--no-print-directory"
                                       "--eval=print-objs: ; @echo $(OBJS)" "print-objs")
                                 :output :string)))
    (loop for name in (split #\Space (string-trim '(#\Linefeed #\Return) objs))
          collect (merge-pathnames name (sbcl-path "src/runtime/")))))

(defmethod asdf:perform ((o shrinkwrap-op) (c asdf:system))
  (destructuring-bind (core elftool run-sbcl cvars) (asdf:input-files o c)
    (destructuring-bind (symbol object bin) (asdf:output-files o c)
      (setf cvars (parse-vars cvars))
      (run run-sbcl "--script" elftool "split" core symbol)
      (run (first (cvar "CC" cvars))
           "-no-pie"
           (remove "-Wl,--export-dynamic" (cvar "LINKFLAGS" cvars) :test #'string=)
           "-Wl,--unresolved-symbols=ignore-in-object-files"
           "-Wl,-rpath,$ORIGIN"
           (cvar "CFLAGS" cvars)
           "-o" bin symbol object (sbcl-objs)
           (link-libraries o cvars))
      (patch-dependencies bin T))))

(defun shrinkwrap (system &rest args &key &allow-other-keys)
  (apply #'asdf:oos 'shrinkwrap-op system args))

(defclass pie-shrinkwrap-op (shrinkwrap-op) ())

(defun sbcl-pic-objs (cvars &key force)
  (loop for file in (sbcl-objs)
        for name = (pathname-name file)
        for pic = (make-pathname :name (format NIL "~a.pic" name) :type "o" :defaults file)
        do (when (or force (not (probe-file pic)))
             (run (first (cvar "CC" cvars))
                  (format NIL "-I~a" (sb-ext:native-namestring (sbcl-path "src/runtime/")))
                  "-fPIC" "-c" (remove "-fno-pie" (cvar "CFLAGS" cvars) :test #'string=)
                  (make-pathname :type "c" :defaults file) "-o" pic))
        collect pic))

(defmethod asdf:perform ((o pie-shrinkwrap-op) (c asdf:system))
  (destructuring-bind (core elftool run-sbcl cvars) (asdf:input-files o c)
    (destructuring-bind (symbol object bin) (asdf:output-files o c)
      (setf cvars (parse-vars cvars))
      (run run-sbcl "--script" elftool "split" "--pie" core symbol)
      (run (first (cvar "CC" cvars))
           "-pie" "-o" bin symbol object (sbcl-pic-objs cvars)
           (link-libraries o cvars)))))

(export-asdf 'shrinkwrap-op)
