(in-package #:org.shirakumo.deploy)

(defvar *info-plist-template* (merge-pathnames "Info.plist" *here*))
(defvar *info-plist-readtable* (copy-readtable))

(set-macro-character #\] (get-macro-character #\) *info-plist-readtable*)
                     NIL *info-plist-readtable*)

(defclass osx-app-deploy-op (deploy-op)
  ())

(defmethod asdf:output-files ((o osx-app-deploy-op) (c asdf:system))
  (destructuring-bind (file) (call-next-method)
    (let ((info (merge-pathnames (format NIL "~a.app/Contents/Info.plist" (asdf:component-name c))
                                 file)))
      (ensure-directories-exist info)
      (with-open-file (out info
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (write-sequence (parse-info-plist c) out)))
    (values (list (merge-pathnames (format NIL "~a.app/Contents/MacOS/" (asdf:component-name c)) file)
                  (merge-pathnames (format NIL "~a.app/Contents/Resources/" (asdf:component-name c)) file))
            T)))

(defmethod output-file :around ((o osx-app-deploy-op))
  (let* ((file (call-next-method))
         (new-dir (append (pathname-directory file)
                          (list (format NIL "~A.app" (pathname-name file))
                                "Contents"
                                "MacOS"))))
    (make-pathname :directory new-dir :defaults file)))

(defun parse-info-plist (system &optional (template *info-plist-template*))
  (with-open-file (in template
                      :direction :input)
    (with-output-to-string (out)
      (loop for c = (read-char in NIL)
            while c
            do (cond ((char= c #\[)
                      (let* ((*readtable* *info-plist-readtable*)
                             (list (read-delimited-list #\] in)))
                        (loop for symbol in list
                              for value = (funcall symbol system)
                              do (when value
                                   (write-sequence
                                    (xml-escape (princ-to-string value))
                                    out)
                                   (return)))))
                     (T
                      (write-char c out)))))))

;; hook ASDF
(flet ((export! (symbol package)
         (import symbol package)
         (export symbol package)))
  (export! 'osx-app-deploy-op :asdf/bundle)
  (export! 'osx-app-deploy-op :asdf))
