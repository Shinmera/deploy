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

(defun checksum-component (component &optional (state (sha3:sha3-init)))
  (declare (type sha3:sha3-state state))
  (when (typep component 'asdf:cl-source-file)
    (checksum-file (asdf:component-pathname component) state :if-does-not-exist NIL))
  (when (typep component 'asdf:parent-component)
    (dolist (child (asdf:component-children component))
      (checksum-component child state)))
  state)

(defun checksum-systems (&optional (state (sha3:sha3-init)))
  (dolist (system (sort (asdf:already-loaded-systems) #'string<) state)
    (checksum-component (asdf:find-system system) state)))

(defun source-checksum ()
  (sha3:sha3-final (checksum-systems) :output-bit-length 224))

(define-hook (:build source-checksum) ()
  (when *source-checksum*
    (setf *source-checksum* (source-checksum)))
  (when *build-time*
    (setf *build-time* (get-universal-time))))
