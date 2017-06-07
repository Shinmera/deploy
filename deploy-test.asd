#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem deploy-test
  :long-name "Deployment Test"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Test system for deployment."
  :homepage "https://github.com/Shinmera/deploy"
  :serial T
  :components ((:file "test"))
  :depends-on (:cl-mpg123
               :cl-out123)
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "deploy-test"
  :entry-point "deploy-test:start")
