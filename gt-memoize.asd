(defsystem :gt-memoize
  :description "Function memoization."
  :version "0.0.1"
  :licence "GPL V3"
  :depends-on (flexi-streams cl-store)
  :components
  ((:static-file "COPYING") (:file "memoize")))
