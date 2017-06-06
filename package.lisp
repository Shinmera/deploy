#|
 This file is a part of deploy
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:deploy
  (:nicknames #:org.shirakumo.deploy)
  (:use #:cl)
  ;; deploy.lisp
  (:export
   #:*data-location*
   #:quit
   #:deploy-op)
  ;; library.lisp
  (:export
   #:*system-source-directories*
   #:list-libraries
   #:ensure-library
   #:library
   #:library-system
   #:library-sources
   #:library-path
   #:library-dont-load-p
   #:library-dont-copy-p
   #:possible-pathnames
   #:find-source-file
   #:library-name
   #:open-library
   #:close-library
   #:library-open-p
   #:define-library)
  ;; toolkit.lisp
  (:export
   #:status
   #:env-set-p
   #:redirect-output
   #:runtime-directory
   #:copy-directory-tree))
