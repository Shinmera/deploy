#|
 This file is a part of deploy
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:deploy-test
  (:nicknames #:org.shirakumo.deploy.test)
  (:use #:cl)
  (:export
   #:start))
(in-package #:org.shirakumo.deploy.test)

(deploy:define-library cl-mpg123-cffi:libmpg123
  :system :cl-mpg123)

(deploy:define-library cl-out123-cffi:libout123
  :system :cl-out123)

(defun start ()
  (let* ((file (or (first (uiop:command-line-arguments))
                   (error "Please pass a file to play.")))
         (file (cl-mpg123:connect (cl-mpg123:make-file file :buffer-size T)))
         (out  (cl-out123:connect (cl-out123:make-output NIL))))
    (format NIL "~&Playback device ~a / ~a" (cl-out123:driver out) (cl-out123:device out))
    (multiple-value-bind (rate channels encoding) (cl-mpg123:file-format file)
      (format NIL "~&Input format ~a Hz, ~a channels, ~a encoded." rate channels encoding)
      (cl-out123:start out :rate rate :channels channels :encoding encoding))
    (unwind-protect
         (loop with buffer = (cl-mpg123:buffer file)
               for read = (cl-mpg123:process file)
               for played = (cl-out123:play out buffer read)
               while (< 0 read)
               do (when (/= played read)
                    (format NIL "~&Playback is not catching up with input by ~a bytes."
                            (- read played))))
      (cl-out123:stop out)
      (cl-out123:disconnect out)
      (cl-mpg123:disconnect file))))
