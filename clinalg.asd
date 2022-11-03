(defsystem "clinalg"
    :description "Matrices and vector data types to interface with C."
    :version "0.0.1"
    :author "Alan Tseng"
    :licence "LGPL3"
    :depends-on ("cffi" "trivial-garbage")
    :serial t
    :components ((:file "array")
                 (:file "print")))
