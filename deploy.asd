(asdf:defsystem deploy
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Tools to aid in the deployment of a fully standalone application."
  :homepage "https://Shinmera.github.io/deploy/"
  :bug-tracker "https://github.com/Shinmera/deploy/issues"
  :source-control (:git "https://github.com/Shinmera/deploy.git")
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
