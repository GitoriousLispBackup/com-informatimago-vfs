
(in-package "COMMON-LISP-USER")

(pushnew (make-pathname :name nil :type nil :version nil
                        :defaults (load-time-value *load-pathname*))
         asdf:*central-registry*)

(ql:quickload :com.informatimago.common-lisp.virtual-file-system)

