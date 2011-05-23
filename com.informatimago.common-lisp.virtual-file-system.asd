;;;; -*- mode:lisp; coding:utf-8 -*-

(asdf:defsystem :com.informatimago.common-lisp.virtual-file-system
    :name "Virtual File System"
    :description  "Implements a RAM-based Virtual File System."
    :author "<PJB> Pascal Bourguignon <pjb@informatimago.com>"
    :version "0.0.1"
    :licence "GPL"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Spring 2011")
                 ((#:albert #:output-dir)          . "../documentation/com.informatimago.common-lisp.virtual-file-system/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    :depends-on (:split-sequence
                 :cl-ppcre
                 :com.informatimago.common-lisp.cesarum)
    :components ((:file "vfs-packages")
                 (:file "virtual-fs"         :depends-on ("vfs-packages"))))

;;;; THE END ;;;;
