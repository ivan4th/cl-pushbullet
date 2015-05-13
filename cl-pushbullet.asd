#-asdf3 (error "walt requires ASDF 3")
(defsystem #:cl-pushbullet
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:cl-pushbullet/pushbullet))
