(defsystem :memoize
  :description "Function memoization."
  :version "0.0.1"
  :licence "Public Domain"
  :depends-on (flexi-streams cl-store)
  :components
  ((:static-file "COPYING") (:file "memoize")))
