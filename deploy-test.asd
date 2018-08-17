#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem deploy-test
  :long-name "Deployment Test"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Test system for deployment."
  :homepage "https://Shinmera.github.io/deploy/"
  :bug-tracker "https://github.com/Shinmera/deploy/issues"
  :source-control (:git "https://github.com/Shinmera/deploy.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:cl-mpg123
               :cl-out123)
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "deploy-test"
  :entry-point "deploy-test:start")
