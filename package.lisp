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
   #:*compression-factor*
   #:quit
   #:deploy-op
   #:discover-entry-point
   #:deployed-p)
  ;; hooks.lisp
  (:export
   #:hook
   #:hook-name
   #:hook-type
   #:hook-function
   #:hook-priority
   #:remove-hook
   #:define-hook
   #:run-hooks
   #:report-error
   #:define-resource-directory)
  ;; library.lisp
  (:export
   #:*system-source-directories*
   #:list-libraries
   #:ensure-library
   #:library
   #:library-system
   #:library-sources
   #:library-path
   #:library-dont-open-p
   #:library-dont-deploy-p
   #:possible-directories
   #:possible-pathnames
   #:find-source-file
   #:library-name
   #:open-library
   #:close-library
   #:library-open-p
   #:define-library)
  ;; osx.lisp
  (:export
   #:*info-plist-template*
   #:osx-app-deploy-op
   #:parse-info-plist)
  ;; toolkit.lisp
  (:export
   #:*data-location*
   #:*status-output*
   #:data-directory
   #:status
   #:env-set-p
   #:redirect-output
   #:runtime-directory
   #:copy-directory-tree))
