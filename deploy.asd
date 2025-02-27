(asdf:defsystem deploy
  :version "3.0.0"
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
               (:file "hooks")
               (:file "library")
               (:file "checksum")
               (:file "deploy")
               (:file "asdf" :if-feature :asdf3)
               (:file "osx" :if-feature :darwin)
               (:file "nx" :if-feature (:and :sbcl :nx))
               (:file "shrinkwrap" :if-feature :sbcl)
               (:file "documentation"))
  :depends-on ((:feature (:not :mezzano) :cffi)
               :sha3
               :documentation-utils
               :pathname-utils
               :trivial-features))
