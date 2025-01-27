(in-package #:org.shirakumo.deploy)

(defvar *source-checksum* T)
(defvar *build-time* T)

(defun checksum-file (file &optional (state (sha3:sha3-init)) &key (if-does-not-exist :error))
  (declare (type sha3:sha3-state state))
  (with-open-file (stream file :element-type '(unsigned-byte 8) :if-does-not-exist if-does-not-exist)
    (when stream
      (let ((buffer (make-array (* 128 1024) :element-type '(unsigned-byte 8))))
        (loop for bytes of-type (unsigned-byte 32) = (read-sequence buffer stream)
              do (sha3:sha3-update state buffer :end bytes)
              until (< bytes (* 128 1024))))))
  state)

(defun list-all-source-files ()
  (let ((files ()))
    #+asdf
    (labels ((rec (component)
               (when (typep component 'asdf:cl-source-file)
                 (push (asdf:component-pathname component) files))
               (when (typep component 'asdf:parent-component)
                 (dolist (child (asdf:component-children component))
                   (rec child)))))
      (dolist (system (sort (asdf:already-loaded-systems) #'string<))
        (rec system)))
    (nreverse files)))

(defun source-checksum ()
  (let ((state (sha3:sha3-init)))
    (dolist (file (list-all-source-files) (sha3:sha3-final state :output-bit-length 224))
      (checksum-file file state :if-does-not-exist NIL))))

(define-hook (:build source-checksum) ()
  (when *source-checksum*
    (setf *source-checksum* (source-checksum)))
  (when *build-time*
    (setf *build-time* (get-universal-time))))
