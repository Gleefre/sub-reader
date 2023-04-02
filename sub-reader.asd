(asdf:defsystem "sub-reader"
  :description "Reader for subtitles"
  :version "0.1.0"
  :author "Gleefre <varedif.a.s@gmail.com>"
  :licence "Apache 2.0"
  :depends-on ("alexandria"
               "simple-scanf"
               "sketch"
               "file-select"
               "stopclock")

  :components ((:file "parser")
               (:file "gui"))

  :defsystem-depends-on (:deploy)
  :build-operation #-darwin "deploy-op" #+darwin "osx-app-deploy-op"
  :build-pathname "sub-reader"
  :entry-point "sub-reader/gui:start")
