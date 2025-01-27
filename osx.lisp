(in-package #:org.shirakumo.deploy)

(defvar *info-plist-template* (merge-pathnames "Info.plist" *here*))
(defvar *info-plist-readtable* (copy-readtable))

(set-macro-character #\] (get-macro-character #\) *info-plist-readtable*)
                     NIL *info-plist-readtable*)

(defclass osx-app-deploy-op (deploy-op)
  ())

(defmethod asdf:output-files ((o osx-app-deploy-op) (c asdf:system))
  (destructuring-bind (file dir) (call-next-method)
    (values (list (merge-pathnames (format NIL "~a.app/Contents/MacOS/" (asdf:component-name c)) file)
                  (merge-pathnames (format NIL "~a.app/Contents/Resources/" (asdf:component-name c)) dir))
            T)))

(define-hook (:deploy osx-app-plist) (directory system op)
  (when (typep op 'osx-app-deploy-op)
    (with-open-file (out (merge-pathnames "../Info.plist" directory)
                         :direction :output
                         :if-exists :supersede)
      (write-sequence (parse-info-plist system) out))))

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
