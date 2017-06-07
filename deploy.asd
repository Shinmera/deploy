#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem deploy
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Tools to aid in the deployment of a fully standalone application."
  :homepage "https://github.com/Shinmera/deploy"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "library")
               (:file "hooks")
               (:file "deploy")
               (:file "osx")
               (:file "documentation"))
  :depends-on (:cffi
               :documentation-utils
               :trivial-features))
