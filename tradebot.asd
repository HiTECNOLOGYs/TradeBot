(defpackage :tradebot-system
  (:use :cl
        :asdf))

(in-package :tradebot-system)

(defsystem :tradebot
  :author "Mark Fedurin <hitecnologys@gmail.com>"
  :depends-on (:drakma
               :jsown
               :ironclad
               :alexandria
               :cl-store)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "filesystem")
               (:file "datatypes")
               (:file "api")
               (:file "actions")))
