(in-package #:org.shirakumo.deploy)

(defparameter *data-location* #p"")
(defparameter *status-output* T)

(defun data-directory ()
  (let ((run (runtime-directory)))
    (make-pathname :host (pathname-host run) :defaults (merge-pathnames *data-location* run))))

(defun find-relative-path-to (target source)
  (let ((directory (list :relative))
        (temp (make-pathname :directory (copy-list (pathname-directory source))
                             :defaults source)))
    (unless (equal (pathname-host target) (pathname-host source))
      (error "Cannot find a relative path from ~a to ~a: different hosts" source target))
    (unless (equal (pathname-device target) (pathname-device source))
      (error "Cannot find a relative path from ~a to ~a: different hosts" source target))
    (loop until (loop for a in (rest (pathname-directory target))
                      for b in (rest (pathname-directory temp))
                      always (equal a b))
          do (unless (rest (pathname-directory temp))
               (error "Cannot find a relative path from ~a to ~a." source target))
             (push :up directory)
             (setf temp (make-pathname :directory (butlast (pathname-directory temp))
                                       :defaults temp)))
    (loop for dir in (nthcdr (length (pathname-directory temp))
                             (pathname-directory target))
          do (push dir directory))
    (make-pathname :directory (nreverse directory))))

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

(defun discover-subdirectories (path)
  (labels ((r (path)
             (list* path (mapc #'r (uiop:subdirectories path)))))
    (r (uiop:pathname-directory-pathname path))))

(defun status (level format-string &rest format-args)
  (when *status-output*
    (format *status-output*
            "~& ~a ~?~%" (case level
                           (0 "==>")
                           (1 "  ->")
                           (T "    >"))
            format-string format-args)))

(defun env-set-p (envvar)
  (let ((value (uiop:getenv envvar)))
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

(defun runtime-directory ()
  (cond ((and (uiop:featurep :nx) (deployed-p))
         (make-pathname :device "rom" :directory '(:absolute)))
        ((uiop:argv0)
         (uiop:truenamize (uiop:pathname-directory-pathname (uiop:argv0))))
        (T
         (uiop:truenamize #p""))))

(defun directory-contents (path)
  (uiop:directory* (merge-pathnames uiop:*wild-file* path)))

(defun copy-directory-tree (source target &key (copy-root T) (exclude (constantly NIL)) (if-exists :supersede))
  (labels ((r (path destination)
             (cond ((uiop:directory-pathname-p path)
                    (let ((tpath (merge-pathnames (format NIL "~a/" (car (last (pathname-directory path))))
                                                  destination)))
                      (dolist (subpath (directory-contents path))
                        (r subpath tpath))))
                   ((not (funcall exclude path destination))
                    (ensure-directories-exist destination)
                    (let ((destination (make-pathname :name (pathname-name path)
                                                      :type (pathname-type path)
                                                      :defaults destination)))
                      (if (probe-file destination)
                          (ecase if-exists
                            (:update
                             (when (< (file-write-date destination) (file-write-date path))
                               (uiop:copy-file path destination)))
                            ((:replace :supersede :overwrite) 
                             (uiop:copy-file path destination))
                            ((NIL :ignore))
                            (:error
                             (error "The file~%  ~a~%already exists." destination)))
                          (uiop:copy-file path destination)))))))
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
           (member (string system-spec) *features* :key #'string :test #'string=))
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

(defun split (split string)
  (let ((out (make-string-output-stream))
        (pieces ()))
    (flet ((add ()
             (let ((string (get-output-stream-string out)))
               (unless (string= string "")
                 (push string pieces)))))
      (loop for c across string
            do (if (char= c split)
                   (add)
                   (write-char c out))
            finally (add))
      (nreverse pieces))))

(defun env-paths (variable)
  (mapcar (lambda (s) (uiop:parse-native-namestring (format NIL "~a/" s)))
          (split #+windows #\; #-windows #\: (or (uiop:getenv variable) ""))))
