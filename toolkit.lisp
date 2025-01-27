(in-package #:org.shirakumo.deploy)

(defvar *here* #.(make-pathname :name NIL :type NIL :defaults (or *compile-file-pathname* *load-pathname*)))
(defparameter *data-location* #p"")
(defparameter *status-output* T)

(defun data-directory ()
  (let ((run (runtime-directory)))
    (make-pathname :host (pathname-host run) :defaults (merge-pathnames *data-location* run))))

(defun make-lib-pathname (name)
  (make-pathname :name (string name)
                 :type #+(and unix (not darwin)) "so"
                       #+darwin "dylib"
                       #+windows "dll"
                       #+nx "nro"
                       #-(or unix darwin windows nx)
                       (error "Unknown target platform. No idea what its shared library pathname is!")))

(defun pathname-filename (path)
  (format NIL "~a~@[.~a~]"
          (pathname-name path) (pathname-type path)))

(defun status (level format-string &rest format-args)
  (when *status-output*
    (format *status-output*
            "~& ~a ~?~%" (case level
                           (0 "==>")
                           (1 "  ->")
                           (T "    >"))
            format-string format-args)))

(defun query-for-library-path ()
  (format *query-io* "~&[DEPLOY] Enter the library path: ")
  (finish-output *query-io*)
  (list (pathname-utils:parse-native-namestring (read-line *query-io*))))

(defun command-line-arguments ()
  #+abcl ext:*command-line-argument-list*
  #+allegro (sys:command-line-arguments)
  #+(or clasp ecl) (loop :for i :from 0 :below (si:argc) :collect (si:argv i))
  #+clisp (coerce (ext:argv) 'list)
  #+clozure ccl:*command-line-argument-list*
  #+cmucl  extensions:*command-line-strings*
  #+mezzano nil
  #+lispworks sys:*line-arguments-list*
  #+sbcl sb-ext:*posix-argv*
  ())

(defun getenv (x)
  #+(or abcl clasp clisp ecl xcl) (ext:getenv x)
  #+allegro (sys:getenv x)
  #+clozure (ccl:getenv x)
  #+cmucl (unix:unix-getenv x)
  #+lispworks (lispworks:environment-variable x)
  #+sbcl (sb-ext:posix-getenv x)
  #-(or abcl clasp clisp ecl xcl allegro clozure cmucl lispworks sbcl)
  NIL)

(defun envvar-directory (var)
  (let ((var (getenv var)))
    (when (and var (string/= "" var))
      (pathname-utils:parse-native-namestring var :as :directory :junk-allowed T))))

(defun envvar-directories (variable)
  (mapcar (lambda (s) (pathname-utils:parse-native-namestring s :as :directory))
          (split #+windows #\; #-windows #\: (or (getenv variable) ""))))

(defun env-set-p (envvar)
  (let ((value (getenv envvar)))
    (when (and value (string/= "" value))
      value)))

(defun redirect-output (target)
  (let ((file (open target
                    :direction :output
                    :if-exists :append
                    :if-does-not-exist :create)))
    (setf *standard-output* file)
    (setf *error-output* file)
    (setf *trace-output* file)
    (setf *debug-io* (make-echo-stream *debug-io* file))
    (multiple-value-bind (ss mm hh d m y) (decode-universal-time (get-universal-time))
      (status 0 "Deploy log started on ~d.~2,'0d.~2,'0d ~d:~2,'0d:~2,'0d"
              y m d hh mm ss))
    file))

(defun featurep (feature)
  (member feature *features* :test #'string=))

(defun runtime-directory ()
  (cond ((and (featurep :nx) (deployed-p))
         (make-pathname :device "rom" :directory '(:absolute)))
        ((first (command-line-arguments))
         (pathname-utils:to-directory
          (pathname-utils:parse-native-namestring (first (command-line-arguments)))))
        (T
         *default-pathname-defaults*)))

(defun directory-contents (path &key (type :wild))
  (directory (make-pathname :name :wild :type type :defaults path)))

(defun copy-file (source target &key (if-exists :supersede))
  (flet ((copy-file (source target)
           (with-open-file (in source :element-type '(unsigned-byte 8))
             (with-open-file (out target :element-type '(unsigned-byte 8) :if-exists :supersede)
               (when out (loop with buffer = (make-array 4096 :element-type '(unsigned-byte 8))
                               for read = (read-sequence buffer in)
                               while (< 0 read)
                               do (write-sequence buffer out :end read)))))))
    (if (probe-file target)
        (ecase if-exists
          (:update
           (when (< (file-write-date target) (file-write-date source))
             (copy-file source target)))
          ((:replace :supersede :overwrite) 
           (copy-file source target))
          ((NIL :ignore))
          (:error
           (error "The file~%  ~a~%already exists." target)))
        (copy-file source target))))

(defun copy-directory-tree (source target &key (copy-root T) (exclude (constantly NIL)) (if-exists :supersede))
  (labels ((r (path destination)
             (cond ((pathname-utils:directory-p path)
                    (let ((tpath (merge-pathnames (format NIL "~a/" (car (last (pathname-directory path))))
                                                  destination)))
                      (dolist (subpath (directory-contents path))
                        (r subpath tpath))))
                   ((not (funcall exclude path destination))
                    (ensure-directories-exist destination)
                    (let ((destination (make-pathname :name (pathname-name path)
                                                      :type (pathname-type path)
                                                      :defaults destination)))
                      (copy-file path destination :if-exists if-exists))))))
    (if copy-root
        (r source target)
        (dolist (subpath (directory-contents source))
          (r subpath target)))))

(defun xml-escape (string)
  (with-output-to-string (out)
    (loop for c across string
          do (case c
               (#\< (format out "&lt;"))
               (#\> (format out "&gt;"))
               (#\& (format out "&amp;"))
               (T (write-char c out))))))

(defun system-applicable-p (system-spec)
  (or (eql T system-spec)
      (and (symbolp system-spec)
           (featurep (string-upcase system-spec)))
      (and (consp system-spec)
           (ecase (first system-spec)
             ((or :or)
              (loop for sub in (rest system-spec)
                    thereis (system-applicable-p sub)))
             ((and :and)
              (loop for sub in (rest system-spec)
                    always (system-applicable-p sub)))
             ((not :not)
              (not (system-applicable-p (second system-spec))))))))

(defun resolve-cffi-spec (spec)
  (labels ((resolve-inner-spec (spec)
             (etypecase spec
               ((or string pathname)
                (list (pathname spec)))
               (cons
                (ecase (first spec)
                  (:or
                   (loop for other in (rest spec)
                         append (resolve-inner-spec other)))
                  (:framework
                   (warn "Deploy does not currently support Darwin frameworks.")
                   ())
                  (:default
                   (list (make-lib-pathname (second spec)))))))))
    (loop for (system-spec spec) in spec
          when (system-applicable-p system-spec)
          append (resolve-inner-spec spec))))

(defun split (split string &key (start 0) (end (length string)))
  (let ((out (make-string-output-stream))
        (pieces ()))
    (flet ((add ()
             (let ((string (get-output-stream-string out)))
               (unless (string= string "")
                 (push string pieces)))))
      (loop for i from start below end
            for c = (char string i)
            do (if (char= c split)
                   (add)
                   (write-char c out))
            finally (add))
      (nreverse pieces))))

(defun parse-vars (file)
  (with-open-file (stream file)
    (remove NIL
            (loop for line = (read-line stream NIL NIL)
                  while line
                  collect (let ((pos (position #\= line)))
                            (when pos
                              (cons (subseq line 0 pos)
                                    (split #\ (subseq line (1+ pos))))))))))
