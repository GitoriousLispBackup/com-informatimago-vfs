;;;; -*- package: COM.INFORMATIMAGO.COMMON-LISP.VIRTUAL-FILE-SYSTEM -*-
;;;;**************************************************************************
;;;;FILE:               virtual-fs.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package implements a RAM based virtual file system.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-04-26 <PJB> Created.  Quick-and-Dirty.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2006
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************

(in-package "COM.INFORMATIMAGO.COMMON-LISP.VIRTUAL-FILE-SYSTEM")

;; (do-external-symbols (symbol "COMMON-LISP") 
;;   (export (intern (string symbol) *package*)))


(defun proper-list-p (object)
  (labels ((proper (current slow)
             (cond ((null current)       t)
                   ((atom current)       nil)
                   ((null (cdr current)) t)
                   ((atom (cdr current)) nil)
                   ((eq current slow)    nil)
                   (t                    (proper (cddr current) (cdr slow))))))
    (proper object (cons nil object))))

(defun test-proper-list-p ()
  (assert
   (every 
    (function identity)
    (mapcar (lambda (test) (eq (first test) (proper-list-p (second test))))
            '((nil x)
              (t ())
              (t (a))
              (t (a b))
              (t (a b c))
              (t (a b c d))
              (nil (a . x))
              (nil (a b . x))
              (nil (a b c . x))
              (nil (a b c d . x))
              (nil #1=(a . #1#))
              (nil #2=(a b . #2#))
              (nil #3=(a b c . #3#))
              (nil #4=(a b c d . #4#))
              (nil (1 . #1#))
              (nil (1 2 . #1#))
              (nil (1 2 3 . #1#))
              (nil (1 2 3 4 . #1#))
              (nil (1 . #2#))
              (nil (1 2 . #2#))
              (nil (1 2 3 . #2#))
              (nil (1 2 3 4 . #2#))
              (nil (1 . #3#))
              (nil (1 2 . #3#))
              (nil (1 2 3 . #3#))
              (nil (1 2 3 4 . #3#))
              (nil (1 . #4#))
              (nil (1 2 . #4#))
              (nil (1 2 3 . #4#))
              (nil (1 2 3 4 . #4#)))))))


(defun unsplit-string (string-list &optional (separator " "))
  "
DO:         The inverse than split-string.
            If no separator is provided then a simple space is used.
SEPARATOR:  (OR NULL STRINGP CHARACTERP)
"
  (check-type separator (or string character symbol) "a string designator.")
  (if string-list
      (with-output-to-string (*standard-output*)
        (princ (pop string-list))
        (dolist (item string-list)
          (princ separator) (princ item)))
      ""))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; regular expressions
;;;

(defun re-compile (re &key extended)
  #+clisp
  (regexp:regexp-compile   re :extended      extended)
  #+(and (not clisp) cl-ppcre)
  (cl-ppcre:create-scanner re :extended-mode extended)
  #-(or clisp cl-ppcre)
  (error "Please implement RE-COMPILE"))

(defun re-exec (re string)
  #+clisp
  (regexp:regexp-exec re string)
  #+(and (not clisp) cl-ppcre)
  (multiple-value-bind (start end starts ends) (cl-ppcre:scan re string)
    (and start end
         (values-list  (cons (cons start end)
                             (map 'list (lambda (s e)
                                          (if (or s e)
                                              (cons s e)
                                              nil))
                                  starts ends)))))
  #-(or clisp cl-ppcre)
  (error "Please implement RE-EXEC"))

(defun re-match-string (string match)
  #+clisp
  (regexp:match-string string match)
  #+(and (not clisp) cl-ppcre)
  (subseq string (car match) (cdr match))
  #-(or clisp cl-ppcre)
  (error "Please implement RE-MATCH-STRING"))

(defun re-match (regexp string)
  (re-exec (re-compile regexp :extended t) string))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 19. Filenames
;;; http://www.lispworks.com/documentation/HyperSpec/Body/19_.htm

(define-condition simple-file-error (file-error simple-error)
  ())

(defparameter *logical-pathname-regexp*
  (let ((host "(([-A-Z0-9]+):)?")
        (dire "(;)?(([-*A-Z0-9]+;|\\*\\*;)*)")
        (name "([-*A-Z0-9]+)?")
        (type "(.([-*A-Z0-9]+)(.([0-9]+|newest|NEWEST|\\*))?)?"))
    (re-compile (concatenate 'string "^" host dire name type "$")
                :extended t)))

(defun parse-logical-pathname (string)
  (flet ((wild (item part wild-inferiors-p)
           (cond ((string= "*"  item) :wild)
                 ((and wild-inferiors-p (string= "**" item)) :wild-inferiors)
                 ((search  "**" item) 
                  (error "Invalid ~A part: ~S; ~
                                \"**\" inside a wildcard-world is forbidden."
                         part item))
                 ((position #\* item) (list :wild-word item))
                 (t item))))
    (multiple-value-bind (all
                          dummy0 host 
                          relative directories dummy1
                          name
                          dummy2 type dummy3 version) 
        (re-exec *logical-pathname-regexp* string)
      (if all
          (list (and host        (re-match-string string host))
                (if relative :relative :absolute)
                (and directories
                     (mapcar
                      (lambda (item) (wild item "directory" t))
                      (butlast (split-sequence #\; (re-match-string
                                                    string directories)))))
                (and name
                     (let ((item (re-match-string string name)))
                       (wild item "name" nil)))
                (and type        
                     (let ((item (re-match-string string type)))
                       (wild item "type" nil)))
                (and version
                     (let ((version (re-match-string string version)))
                       (cond 
                         ((string= "*" version) :wild)
                         ((string-equal "NEWEST" version) :newest)
                         (t (parse-integer version :junk-allowed nil))))))
          (error "Syntax error parsing pathname ~S" string)))))


(defun match-wild-word-p (item wild)
  (flet ((concat (type list)
           (let* ((totlen  (reduce (lambda (length item) (+ (length item) length))
                                   list :initial-value 0))
                  (result  (cond
                             ((or (eq type 'string)
                                  (and (consp type) (eq 'string (first type))))
                              (make-string totlen))
                             ((or (eq type 'vector)
                                  (and (consp type) (eq 'vector (first type)))
                                  (eq type 'array)
                                  (and (consp type) (eq 'array (first type))))
                              (make-array totlen))
                             ((eq type 'list)
                              (make-list totlen))
                             (t (error "Invalid sequence type: ~S" type)))))
             (loop 
                :for item :in list
                :and start = 0 :then (+ start (length item))
                :do (replace result item :start1 start)
                :finally (return result)))))
    (re-match 
     (concat 'string
             (cons "^"
                   (nconc
                    (loop 
                       :for chunks :on (split-sequence #\* wild) 
                       :collect (car chunks) :when (cdr chunks) :collect ".*")
                    (list "$"))))
     item)))



(defclass pathname ()
  ((host      :accessor %pathname-host
              :initarg :host
              :initform nil)
   (device    :accessor %pathname-device
              :initarg :device
              :initform :unspecific)
   (directory :accessor %pathname-directory
              :initarg :directory
              :initform nil)
   (name      :accessor %pathname-name
              :initarg :name
              :initform nil)
   (type      :accessor %pathname-type
              :initarg :type
              :initform nil)
   (version   :accessor %pathname-version
              :initarg :version
              :initform nil)))

(defmethod print-object ((self pathname) stream)
  (flet ((present-item (item)
           (cond ((null item) item)
                 ((listp item) (second item))
                 ((eq :wild item) "*")
                 ((eq :wild-inferiors item) "**")
                 (t item))))
    #+(or)
    (dolist (s '(*print-array* *print-base* *print-case*
                 *print-circle* *print-escape* *print-gensym* *print-length*
                 *print-level* *print-lines* *print-miser-width*
                 *print-pprint-dispatch* *print-pretty* *print-radix*
                 *print-readably* *print-right-margin*))  
      (format t "~A = ~A~%" s (symbol-value s)))
    (format stream "~:[~;#P\"~]~A:~:[~;;~]~{~A;~}~:[~;~:*~A~]~
                    ~:[~;.~:*~A~:[~;.~:*~A~]~]~8:*~:[~;\"~]"
            *print-escape*
            (pathname-host self)
            (eq :relative (first (pathname-directory self)))
            (mapcar (function present-item) (rest (pathname-directory self)))
            (present-item (pathname-name self))
            (present-item (pathname-type self))
            (present-item (pathname-version self))))
  self)


(defun assert-type (datum expected-type)
  (or (typep datum expected-type)
      (error (make-condition 'type-error
                             :datum datum :expected-type expected-type))))

(defmacro define-pathname-attribute (name)
  `(defun ,(intern (format nil "PATHNAME-~A" name))
       (pathname &key (case :local)) 
     (,(intern (format nil "%PATHNAME-~A" name)) (pathname pathname))))

(define-pathname-attribute host)
(define-pathname-attribute device)
(define-pathname-attribute directory)
(define-pathname-attribute name)
(define-pathname-attribute type)
(define-pathname-attribute version)

#||

Pathname Host Component

The name of the file system on which the file resides, or the name
of a logical host.


Pathname Device Component

Corresponds to the ``device'' or ``file structure'' concept in
many host file systems: the name of a logical or physical device
containing files.


Pathname Directory Component

Corresponds to the ``directory'' concept in many host file
systems: the name of a group of related files. 


Pathname Name Component

The ``name'' part of a group of files that can be thought of as
conceptually related. 


Pathname Type Component

Corresponds to the ``filetype'' or ``extension'' concept in many
host file systems. This says what kind of file this is. This
component is always a string, nil, :wild, or :unspecific. 


Pathname Version Component

Corresponds to the ``version number'' concept in many host file systems.

The version is either a positive integer or a symbol from the
following list: nil, :wild, :unspecific, or :newest (refers to the
largest version number that already exists in the file system when
reading a file, or to a version number greater than any already
existing in the file system when writing a new
file). Implementations can define other special version symbols. 

||#


(defclass logical-pathname (pathname)
  ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 21. Streams
;;; http://www.lispworks.com/documentation/HyperSpec/Body/21_.htm

(define-condition simple-stream-error (stream-error simple-error)
  ())

(defclass stream ()
  ((open-p          :accessor %open-stream-p
                    :initarg  :open-p)
   (element-type    :accessor %stream-element-type
                    :initarg  :element-type
                    :initform 'character)
   (external-format :accessor %stream-external-format
                    :initarg  :external-format
                    :initform :default)
   (input-p         :accessor %input-stream-p
                    :initarg  :input-p)
   (output-p        :accessor %output-stream-p
                    :initarg  :output-p)))


(defclass broadcast-stream (stream)
  ((streams :accessor %broadcast-stream-streams
            :initarg :streams
            :initform nil)))

(defclass concatenated-stream (stream)
  ((streams :accessor %concatenated-stream-streams
            :initarg :streams
            :initform nil)))

(defclass echo-stream (stream)
  ((input-stream  :accessor %echo-stream-input-stream
                  :initarg :input-stream
                  :initform nil)
   (output-stream :accessor %echo-stream-output-stream
                  :initarg :output-stream
                  :initform nil)))

(defclass file-stream (stream)
  ((pathname :accessor pathname 
             :initarg :pathname)
   (file     :accessor %file-stream-file
             :initarg :file)
   (contents :accessor %file-stream-contents
             :initarg :contents)
   (position :accessor %file-stream-position
             :initarg :position
             :initform 0
             :type    (integer 0))))

(defclass file-input-stream (file-stream)
  ())

(defclass file-output-stream (file-stream)
  ())

(defclass file-io-stream (file-input-stream  file-output-stream)
  ())

(defclass string-stream (stream)
  ())

(defclass string-input-stream (string-stream)
  ((string :accessor %string-stream-input-string
           :initarg :string
           :initform ""
           :type     string)
   (index  :accessor %string-stream-index
           :initarg :index
           :initform 0
           :type (integer 0))
   (start  :accessor %string-stream-start
           :initarg :start
           :initform 0
           :type (integer 0))
   (end    :accessor %string-stream-end
           :initarg :end
           :initform nil
           :type (or null (integer 0)))))

(defclass string-output-stream (string-stream)
  ((string :accessor %string-stream-output-string
           :initarg :string
           :initform (make-array 8
                                 :fill-pointer 0 :adjustable t
                                 :element-type 'character)
           :type     string)))

(defclass synonym-stream (stream)
  ((symbol  :accessor %synonym-stream-symbol
            :initarg :symbol)))

(defclass two-way-stream (stream)
  ((input-stream  :accessor %two-way-stream-input-stream
                  :initarg :input-stream
                  :initform nil)
   (output-stream :accessor %two-way-stream-output-stream
                  :initarg :output-stream
                  :initform nil)))



(defclass cl-stream (stream)
  ((stream :accessor cl-stream-stream
           :initarg :cl-stream)))

(defun cl-stream (stream)  (make-instance 'cl-stream :cl-stream stream))
(defparameter *debug-io*        (cl-stream cl:*debug-io*))
(defparameter *error-output*    (cl-stream cl:*error-output*))
(defparameter *trace-output*    (cl-stream cl:*trace-output*))
(defparameter *standard-output* (cl-stream cl:*standard-output*))
(defparameter *standard-input*  (cl-stream cl:*standard-input*))
(defparameter *terminal-io*     (cl-stream cl:*terminal-io*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 19. Filenames
;;; http://www.lispworks.com/documentation/HyperSpec/Body/19_.htm


(defmethod pathname ((pathspec t))
  (assert-type pathspec '(or string file-stream pathname)))

(defmethod pathname ((pathspec pathname))
  pathspec)

(defmethod pathname ((pathspec string))
  (destructuring-bind (host relative directory name type version)
      (parse-logical-pathname pathspec)
    ;; (print (list host relative directory name type version))
    (make-instance 'pathname :host host :directory (cons relative directory)
                   :name name :type type :version version)))


(defun install-pathname-reader-macro (&optional (readtable *readtable*))
  (set-dispatch-macro-character #\# #\p
    (lambda (stream disp char)
      (declare (ignore disp char))
      (pathname (read stream t nil t)))
    readtable))


(defun reset-readtable ()
  (setq *readtable* (copy-readtable nil)))

(defun check-host (host)
  (cond
    ((null host) (name *default-file-system*))
    ((file-system-named host) host)
    (t (error "Invalid host ~S" host))))

(defun make-pathname (&key host device directory name type version (case :local)
                      (defaults nil defaults-p))
  (cond ((stringp directory)  (setf directory (list :absolute directory)))
        ((eq :wild directory) (setf directory (list :absolute :wild-inferiors))))
  (make-instance 'pathname
    :host (check-host (or host (if defaults-p
                                   (and defaults (pathname-host      defaults))
                                   (pathname-host *default-pathname-defaults*))))
    :device      (or device    (and defaults (pathname-device    defaults)))
    :directory   (or directory (and defaults (pathname-directory defaults)))
    :name        (or name      (and defaults (pathname-name      defaults)))
    :type        (or type      (and defaults (pathname-type      defaults)))
    :version     (or version   (and defaults (pathname-version   defaults)))))


(defun pathnamep (object) (typep object 'pathname))



(defparameter *logical-pathname-translations* 
  (make-hash-table :test (function equal)))

(defun logical-pathname-translations (host)
  (assert-type host 'string)
  (gethash host  *logical-pathname-translations*))

(defun (setf logical-pathname-translations) (value host)
  (assert-type host 'string)
  (assert (and (proper-list-p value)
               (every (lambda (item)
                        (and (proper-list-p item)
                             (typep (first  item) '(or string logical-pathname))
                             (typep (second item) '(or string pathname))))
                      value)))
  (setf (gethash host  *logical-pathname-translations*) value))


(defun load-logical-pathname-translations (host)
  (assert-type host 'string)
  (if (nth-value 1 (logical-pathname-translations host))
      nil
      (with-open-file (input (make-pathname :host "SYS"
                                            :directory '(:absolute "SITE")
                                            :name host
                                            :type "TRANSLATIONS"
                                            :version :newest)
                             :if-does-not-exist nil)
        (if input
            (setf (logical-pathname-translations host) (read input nil nil))
            (error "No logical pathname translation file found for host ~S"
                   host)))))


(defun logical-pathname (pathspec)
  (pathname pathspec))

(defun parse-namestring (thing &optional host
                         (default-pathname *default-pathname-defaults*)
                         &key (start 0) (end nil) (junk-allowed nil))
  (when (typep thing 'file-stream)
    (setf thing (pathname thing)))
  (error "parse-namestring not implemented yet"))


(defun wild-pathname-p (pathname &optional field-key)
  (assert-type pathname '(or pathname string file-stream))
  (setf pathname (pathname pathname))
  (flet ((wild-p (item)
           (or (eq item :wild)
               (eq item :wild-inferiors)
               (and (consp item) 
                    (eq (first item) :wild-word)))))
    (if (null field-key)
        (or (wild-pathname-p pathname :host)
            (wild-pathname-p pathname :device)
            (wild-pathname-p pathname :directory)
            (wild-pathname-p pathname :name)
            (wild-pathname-p pathname :type)
            (wild-pathname-p pathname :version))
        (ecase field-key
          (:host    (wild-p (pathname-host    pathname)))
          (:device  (wild-p (pathname-device  pathname)))
          (:directory (some (function wild-p)
                            (cdr (pathname-directory pathname))))
          (:name    (wild-p (pathname-name    pathname)))
          (:type    (wild-p (pathname-type    pathname)))
          (:version (wild-p (pathname-version pathname)))))))








(defun match-item-p (item wild &optional match-wild-word-p)
  (or (eq wild :wild)
      (and (consp wild) (eq (first wild) :wild-word)
           match-wild-word-p (match-wild-word-p item (second wild)))
      (eq item wild)
      (and (stringp item) (stringp wild) (string= item wild))))

(defun match-directory-items-p (item wild)
  (or (null item wild)
      (if (eq (first wild) :wild-inferiors) 
          (loop
             :for rest :on item
             :thereis (match-directory-items-p rest (rest wild)))
          (and (match-item-p (first item) (first wild) t)
               (match-directory-items-p (rest item) (rest wild))))))

(defun pathname-match-p (pathname wildcard)
  (assert-type pathname '(or pathname string file-stream))
  (assert-type wildcard '(or pathname string file-stream))
  (setf pathname (pathname pathname))
  (setf wildcard (merge-pathnames (pathname wildcard)
                                  (make-pathname
                                   :host :wild
                                   :device :wild
                                   :directory :wild
                                   :name :wild
                                   :type :wild
                                   :version :wild)))
  (and (match-item-p (pathname-host    item) (pathname-host    wild) t)
       (match-item-p (pathname-device  item) (pathname-device  wild) t)
       (match-item-p (pathname-name    item) (pathname-name    wild) t)
       (match-item-p (pathname-type    item) (pathname-type    wild) t)
       (match-item-p (pathname-version item) (pathname-version wild) nil)
       (or (and (eq :absolute (first (pathname-directory wild)))
                (eq :relative (first (pathname-directory item)))
                (eq :wild-inferiors (second  (pathname-directory wild))))
           (and (eq (first (pathname-directory wild))
                    (first (pathname-directory item)))
                (match-directory-items-p (rest (pathname-directory item))
                                         (rest (pathname-directory wild)))))))


(defun translate-logical-pathname (pathname &key)
  (warn "translate-logical-pathname not implemented yet")
  (pathname pathname))


(defun translate-pathname (source from-wildcard to-wildcard &key)
  (error "translate-pathname not implemented yet"))

(defun delete-back (dir)
  (loop
     :with changed = t
     :while changed
     :do (loop 
            :for cur = dir :then (cdr cur)
            :initially (setf changed nil)
            :do (when (and (or (stringp (cadr cur)) (eq :wild (cadr cur)))
                           (eq :back (caddr cur)))
                  (setf (cdr cur) (cdddr cur)
                        changed t)))
     :finally (return dir)))
           
(defun merge-pathnames (pathname 
                        &optional (default-pathname *default-pathname-defaults*)
                        (default-version :newest))
  (setf pathname (pathname pathname))
  (make-pathname 
   :host    (or (pathname-host pathname) (pathname-host default-pathname))
   :device  (if (and (stringp (pathname-host pathname))
                     (stringp (pathname-host default-pathname))
                     (member (pathname-device pathname) '(:unspecific nil))
                     (string= (pathname-host pathname)
                              (pathname-host default-pathname)))
                (pathname-device default-pathname)
                (or (pathname-device pathname) :unspecific))
   :directory (if (eq :relative (car (pathname-directory pathname)))
                  (delete-back
                   (append (pathname-directory default-pathname)
                           (copy-list (cdr (pathname-directory pathname)))))
                  (or (pathname-directory pathname)
                      (pathname-directory default-pathname)))
   :name    (or (pathname-name pathname) (pathname-name default-pathname))
   :type    (or (pathname-type pathname) (pathname-type default-pathname))
   :version (cond ((pathname-name pathname)
                   (or (pathname-version pathname) default-version))
                  ((null default-version)
                   (pathname-version pathname))
                  (t
                   (or (pathname-version pathname) 
                       (pathname-version default-pathname))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internals: Memory based file systems.

(deftype file-type        () '(or string null))
(deftype pathname-type    () '(or file-type (member :wild :unspecific)))
(deftype file-version     () 'integer)
(deftype pathname-version () '(or file-version
                               (member nil :wild :unspecific :newest)))

(defclass fs-item ()
  ((parent :accessor parent
           :initarg :parent 
           :type (or null fs-directory)))
  (:documentation "Either a file, a directory or a whole file system."))

(defmethod pathname ((self fs-item))
  (error "PATHNAME not defined for the abstract class FS-ITEM"))

(defmethod dump ((self fs-item)
                 &optional (stream *standard-output*) (level ""))
  (format t "~A--> [ITM] ~S ### CANNOT DUMP SUCH AN ITEM~%" level self))


(defclass fs-directory (fs-item)
  ((name            :accessor name
                    :initarg :name
                    :type     string)
   (entries         :accessor entries
                    :initarg :entries
                    :initform (make-hash-table :test (function equal))
                    :type     hash-table))
  (:documentation "A directory, mapping item names to subdirectories or files."))

(defmethod pathname ((self fs-directory))
  (let ((path (pathname (parent self))))
    (setf (%pathname-directory path) (nconc (%pathname-directory path)
                                            (list (name self))))
    path))

(defmethod dump ((self fs-directory)
                 &optional (stream *standard-output*) (level ""))
  (format t "~A--> [DIR] ~A~%" level (name self))
  (maphash (lambda (k v)
             (declare (ignore k))
             (dump v stream (concatenate 'string level "   |")))
           (entries self)))

(defmethod select-entries ((self t) predicate) 
  '())
(defmethod select-entries ((self fs-directory) predicate)
  (let ((result '()))
    (maphash (lambda (k v) 
               (declare (ignore k))
               (when (funcall predicate v) (push v result))) (entries self))
    result))

(defmethod entry-name ((self fs-directory)) 
  (name self))

(defmethod entry-named ((self t) (name string))
  (error "~A is not a directory" (pathname self)))

(defmethod entry-named ((self fs-directory) (name string))
  (gethash name (entries self)))


(defmethod entry-at-path ((self t) path)
  (error "~S is not a directory" self))

(defmethod entry-at-path ((self fs-directory) path)
  (if (null path)
      self
      (let ((entry (entry-named self (car path))))
        (if (null entry)
            (error "There's no directory ~A~A;" (pathname self) (car path))
            (entry-at-path entry (cdr path))))))


(defmethod add-entry ((self fs-directory) (entry fs-item))
  (let ((name (entry-name entry)))
    (if (entry-named self name)
        (error "An entry named ~S already exists in ~S" name self)
        (setf (parent entry)                self
              (gethash name (entries self)) entry))))

(defmethod remove-entry-named ((self fs-directory) (name string))
  (if (entry-named self name)
      (remhash name (entries self))
      (error "No entry named ~S exists in ~S" name self)))
  

(defclass fs-file (fs-item)
  ((name            :accessor name
                    :initarg :name
                    :type     string)
   (type            :accessor type
                    :initarg :type
                    :type     file-type)
   (versions        :accessor versions
                    :initarg :versions
                    :initform (make-hash-table :test (function eql))
                    :type     hash-table)
   (newest          :accessor newest
                    :initarg :newest
                    :initform nil
                    :type    (or null file-contents)))
  (:documentation "A file, with all its versions."))

(defmethod pathname ((self fs-file))
  (let ((path (pathname (parent self))))
    (setf (%pathname-name path)  (name self)
          (%pathname-type path)  (type self))
    path))

(defmethod dump ((self fs-file)
                 &optional (stream *standard-output*) (level ""))
  (format t "~A--> [FIL] ~A.~A~%" level (name self) (type self))
  (maphash (lambda (k v)
             (declare (ignore k))
             (dump v stream (concatenate 'string level "   |")))
           (entries self)))

(defun pathname-entry-name (path)
  (format nil "~A.~A" (pathname-name path) (pathname-type path)))

(defmethod entry-name ((self fs-file)) 
  (format nil "~A.~A" (name self) (type self)))

(defmethod author       ((self fs-file)) (author       (newest self)))
(defmethod write-date   ((self fs-file)) (write-date   (newest self)))
(defmethod element-type ((self fs-file)) (element-type (newest self)))

(defmethod select-versions ((self fs-file) predicate)
  (let ((result '()))
    (maphash (lambda (k v) 
               (declare (ignore k))
               (when (funcall predicate v) (push v result))) (versions self))
    result))

(defclass file-contents (fs-item)
  ((version         :accessor version
                    :initarg :version
                    :type     file-version)
   (author          :accessor author
                    :initarg :author
                    :initform nil
                    :type    (or null string))
   (write-date      :accessor write-date
                    :initarg :write-date
                    :initform (get-universal-time)
                    :type    (or (null (integer 0))))
   (element-type    :accessor element-type
                    :initarg :element-type
                    :initform 'character)
   (contents        :accessor contents
                    :initarg :contents
                    :type     vector))
  (:documentation "A versionned file contents."))

(defmethod pathname ((self file-contents))
  (let ((path (pathname (parent self))))
    (setf (%pathname-version path) (version self))
    path))

(defmethod dump ((self file-contents)
                 &optional (stream *standard-output*) (level ""))
  (format t "~A--> [VER] ~A (:AUTHOR ~S :WRITE-DATE ~S :SIZE ~A)~%"
          level (version self) (author self) (write-date self)
          (length (contents self))))


(defparameter *author* nil "The name or identification of the user.")

(defmethod create-new-version ((self fs-file) &key (element-type 'character))
  (setf (newest self)
        (make-instance 'file-contents
          :version (1+ (if (null (newest self)) 0 (version (newest self))))
          :author *author*
          :write-date (get-universal-time)
          :element-type element-type
          :contents (make-array 0 :fill-pointer 0 :adjustable t
                                :element-type element-type)
          :parent self))
  (setf (gethash (version (newest self)) (versions self)) (newest self))
  self)



(defclass file-system (fs-directory)
  ()
  (:documentation "A file system."))

(defmethod pathname ((self file-system))
  (make-pathname :host (name self) :directory (list :absolute)))

(defparameter *file-systems* (make-hash-table :test (function equal)))

(defun file-system-register (fs)
  (setf (gethash (name fs) *file-systems*) fs))

(defun file-system-named (name)
  (gethash name *file-systems*))

(defparameter *default-file-system* 
  (file-system-register (make-instance 'file-system :name "ROOT")))

(file-system-register (make-instance 'file-system :name "SYS"))
(file-system-register (make-instance 'file-system :name "HOME"))

(defparameter *default-pathname-defaults* 
  (make-pathname :host (name *default-file-system*)
                 :directory '(:absolute)
                 :defaults nil))
                 

(defun decompose-pathname (path)
  (format t "~{~&HOST      = ~S~
               ~&DEVICE    = ~S~
               ~&DIRECTORY = ~S~
               ~&NAME      = ~S~
               ~&TYPE      = ~S~
               ~&VERSION   = ~S~
               ~&~}" (mapcar (lambda (f) (funcall f path))
                             (list (function pathname-host)
                                   (function pathname-device)
                                   (function pathname-directory)
                                   (function pathname-name)
                                   (function pathname-type)
                                   (function pathname-version)))))



(defun resolve-pathspec (pathspec)
  (translate-logical-pathname (pathname pathspec)))

(defun directory-entry (pathspec)
  (let* ((fspath (resolve-pathspec pathspec))
         (fs  (if (pathname-host fspath)
                  (file-system-named (pathname-host fspath))
                  *default-file-system*)))
    (if fs
        (entry-at-path fs (cdr (pathname-directory fspath)))
        (error "There's no file system named ~S" (pathname-host fspath)))))

(defmethod create-directories-at-path ((self fs-directory) path
                                       &optional created)
  (if (null path)
      created
      (let ((entry (entry-named self (car path))))
        (unless entry
          (setf entry (make-instance 'fs-directory :name (car path))
                created t)
          (add-entry self entry))
        (if (typep entry 'fs-directory)
            (create-directories-at-path entry (cdr path) created)
            (error "~A~A; already exists and is not a directory."
                   (pathname self) (car path))))))

(defun file-entry (pathspec)
  (let* ((dir      (directory-entry pathspec))
         (file     (pathname pathspec))
         (entry    (entry-named dir (pathname-entry-name file))))
    (if entry
        (case (pathname-version file)
          ((nil)      entry)
          ((:newest)  (newest entry))
          (otherwise  (gethash (pathname-version file) (versions entry))))
        (error "There's no file ~A" file))))

(defun create-file-at-path (pathspec &optional (create-version-p t))
  (let* ((dir      (directory-entry pathspec))
         (file     (pathname pathspec))
         (entry    (entry-named dir (pathname-entry-name file))))
    (unless entry
      (setf entry (make-instance 'fs-file
                    :name (pathname-name file) :type (pathname-type file)))
      (add-entry dir entry))
    (if (typep entry 'fs-file)
        (if create-version-p 
            (create-new-version entry)
            entry)
        (error "~A already exist and is not a file" (pathname entry)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 20. Files
;;; http://www.lispworks.com/documentation/HyperSpec/Body/20_.htm

(defun collapse-sequences-of-wild-inferiors (list)
  (if (search '(:wild-inferiors :wild-inferiors) list)
      (labels ((collapse (list)
                 (cond ((null list) list)
                       ((and (eq :wild-inferiors (first  list))
                             (eq :wild-inferiors (second list)))
                        (collapse (rest list)))
                       (t (cons (first list) (collapse (rest list)))))))
        (collapse list))
      list))


(defun collect (current dpath fspath)
  (cond
    ((null dpath)
     (if (pathname-name fspath)
         (let ((entries
                (select-entries
                 current
                 (lambda (item)
                   (and (typep item 'fs-file)
                        (match-item-p (name item) (pathname-name fspath) t)
                        (match-item-p (type item) (pathname-type fspath) t))))))
           (if  (pathname-version fspath)
                (mapcan (lambda (item)
                          (select-versions 
                           item
                           (lambda (version) 
                             (match-item-p (version version)
                                           (pathname-version fspath) nil))))
                        entries)
                entries))
         (list current)))
    ((eq :wild-inferiors (car dpath))
     (nconc (mapcan (lambda (item) (collect item dpath fspath))
                    (select-entries current (constantly t)))
            (mapcan (lambda (item) (collect item (rest dpath) fspath))
                    (select-entries current (constantly t)))))
    (t
     (mapcan 
      (lambda (item) (collect item (rest dpath) fspath))
      (select-entries
       current 
       (lambda (item) (and (typep item 'fs-directory)
                           (match-item-p (name item) (car dpath) t))))))))


(defun directory (pathspec &key)
  (let* ((fspath (resolve-pathspec pathspec))
         (fs  (if (pathname-host fspath)
                  (file-system-named (pathname-host fspath))
                  *default-file-system*)))
    (if fs
        (let ((d (cdr (pathname-directory fspath))))
          (mapcar (function pathname)
                  (collect fs (collapse-sequences-of-wild-inferiors d) fspath)))
        (error "Invalid host ~S"  (pathname-host fspath)))))


(defun probe-file (pathspec)
  (values (ignore-errors (pathname (file-entry pathspec)))))

(defun ensure-directories-exist (pathspec &key verbose)
  (let* ((fspath (resolve-pathspec pathspec))
         (fs  (if (pathname-host fspath)
                  (file-system-named (pathname-host fspath))
                  *default-file-system*))
         (dir (if (pathname-name fspath)
                  (pathname-directory fspath)
                  (butlast (pathname-directory fspath)))))
    (if fs
        (values pathspec (create-directories-at-path fs (cdr dir)))
        (error "There's no file system named ~S" (pathname-host fspath)))))

(defun truename (filespec)
  (let ((path (merge-pathnames filespec (make-pathname :version :newest
                                                       :defaults filespec))))
    (or (probe-file path)
        (error (make-condition 'simple-file-error :pathname path
                               :format-control "~A: File ~A does not exist"
                               :format-arguments (list 'truename filespec))))))

(defun file-author       (path) (author       (file-entry (truename path))))
(defun file-write-date   (path) (write-date   (file-entry (truename path))))
(defun file-element-type (path) (element-type (file-entry (truename path))))

(defmethod rename-entry ((self fs-file) newpath)
  ;; rename the whole file
  (when (ignore-errors (probe-file newpath))
    (delete-file newpath))
  (delete-entry self)
  (setf (name self) (pathname-name newpath)
        (type self) (pathname-type newpath))
  (add-entry newdir self)
  self)

(defmethod rename-entry ((self file-contents) newpath)
  ;; rename the version
  (let ((file (if (ignore-errors (probe-file newpath))
                  (file-at-path newpath)
                  (create-file-at-path newpath nil))))
    (remove-version (parent self) (version self))
    (setf (version self) (if (newest file)
                             (max (version self) (1+ (version (newest file))))
                             (version self))
          (parent self)   file
          (gethash (version self) (versions file)) self)
    self))

(defmethod delete-entry ((self fs-file))
  ;; delete the whole file
  (remove-entry-named (parent self) (pathname-entry-name self)))

(defmethod remove-version ((self fs-file) version)
  (remhash version (versions self))
  (when (= version (version (newest self)))
    (let ((maxk -1) (maxv))
      (maphash (lambda (k v) (when (< maxk k) (setf maxk k maxv v))) (versions self))
      (if maxv
          (setf (newest self) maxv)
          ;; otherwise, we've deleted the last version, let's delete the file:
          (delete-entry self)))))
  
(defmethod delete-entry ((self file-contents))
  ;; delete the version ( careful with (newest (parent self)) ).
  (remove-version (parent self) (version self)))

(defun rename-file (filespec new-name)
  (let* ((defaulted (merge-pathnames new-name filespec))
         (old-truename (truename filespec))
         (new-truename (resolve-pathspec defaulted)))
    (print (list defaulted old-truename new-truename))
    (when (wild-pathname-p defaulted)
      (error (make-condition
              'simple-file-error
              :pathname defaulted
              :format-control "~A: source path ~A contains wildcards"
              :format-arguments (list 'rename-file defaulted))))
    (when (wild-pathname-p new-truename)
      (error (make-condition
              'simple-file-error
              :pathname new-truename
              :format-control "~A: target path ~A contains wildcards"
              :format-arguments (list 'rename-file new-truename))))
    (let* ((newpath (make-pathname :version nil :defaults new-truename))
           (newdir  (directory-entry newpath)))
      (unless newdir
        (error (make-condition
                'simple-file-error
                :pathname newpath
                :format-control "~A: target directory ~A doesn't exist"
                :format-arguments (list 'rename-file newpath))))
      (rename-entry (file-entry old-truename) newpath))
    (values defaulted old-truename new-truename)))

(defun delete-file (filespec)
  (delete-entry (file-entry (truename filespec)))
  t)
    
(defun delete-directory (pathspec)
  (let ((dir (directory-entry pathspec)))
    (when dir
      (when (plusp (hash-table-count (entries dir)))
        (error (make-condition
                'simple-file-error
                :pathname newpath
                :format-control "~A: directory ~A is not empty"
                :format-arguments (list 'delete-directory pathspec))))
      (delete-entry dir)))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 21. Streams
;;; http://www.lispworks.com/documentation/HyperSpec/Body/21_.htm



#||
(defun forward-call (function arguments)
  "&optional &key"
  (let ((opt  (position '&optional arguments))
        (rest (position '&rest     arguments))
        (key  (position '&key      arguments)))
    (if rest
        `(apply (function ,function)
                ,@(subseq arguments 0 (or opt rest))
                ,@(when opt
                        (mapcar (lambda (x) (if (listp x) (first x) x)) 
                                (subseq arguments (1+ opt) rest)))
                ,(nth (1+ rest) arguments))
        `(,function
          ,@(subseq arguments 0 (or opt rest))
          ,@(when opt
                  (mapcar (lambda (x) (if (listp x) (first x) x)) 
                          (subseq arguments (1+ opt) rest)))
          ,@(when key
                  (mapcan (lambda (x) 
                            (if (listp x)
                                (list (if (listp (first x))
                                          (first (first x))
                                          (intern (string (first x)) "KEYWORD"))
                                      (if (listp (first x))
                                          (second (first x))
                                          (first x)))
                                (list (intern (string x) "KEYWORD")
                                      x)))
                          (subseq arguments (1+ key) rest)))))))
||#


 

;; (define-forward name arguments
;;   [ documentation-string ]
;;   { declarations }
;;   { forward-method-description })
;;
;; (declare (stream-arguments stream)
;;          (stream-designnator (istream :input)) ; default
;;          (stream-designator  (ostream :output))
;;          (check-stream-type file-stream)
;;          (cl-forward t))
;; 
;; (declare (stream-arguments stream))
;; 
;; (declare (check-stream-type file-stream))
;;
;; method-description ::= (:method class [[declaration* | documentation]] form*)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun make-method-lambda-list (lambda-list self-name self-type)
   (let* ((got-it nil)
          (mand (mapcar (lambda (par) 
                          (let ((name (parameter-name par)))
                            (if (eq name self-name)
                                (progn (setf got-it t)
                                       (list name self-type))
                                (list name 't))))
                        (lambda-list-mandatory-parameters lambda-list)))
          (opti (let ((optionals  (lambda-list-optional-parameters lambda-list)))
                  (cond
                    ((null optionals) nil)
                    (got-it (cons '&optional 
                                  (mapcar (function parameter-specifier)
                                          optionals)))
                    (t (let ((pos  (position self-name optionals
                                             :key (function parameter-name))))
                         (if pos
                             (append
                              (mapcar (lambda (par) (list (parameter-name par) 't))
                                      (subseq optionals 0 pos))
                              (list
                               (list (parameter-name (nth pos optionals))
                                     self-type))
                              (when (< (1+ pos) (length optionals))
                                (cons '&optional 
                                      (mapcar (function parameter-specifier)
                                              (subseq optionals (1+ pos))))))
                             (cons '&optional 
                                   (mapcar (function parameter-specifier)
                                           optionals))))))))
          (keys (mapcar (function parameter-specifier)
                        (lambda-list-keyword-parameters lambda-list)))
          (rest (and (lambda-list-rest-p lambda-list)
                     (mapcar (function parameter-specifier)
                             (lambda-list-rest-parameter lambda-list)))))
     (append mand opti
             (when keys (cons '&key keys))
             (when rest (list '&rest rest))))))

            
(defun stream-designator (stream direction)
  "DIRECTION is either *standard-input* or *standard-output*"
  (case stream 
    ((t)       *terminal-io*)
    ((nil)     direction)
    (otherwise stream)))

(defun raise-type-error (object type)
  (error (make-condition 'type-error :datum object :expected-type type)))


(eval-when (:compile-toplevel #| Not necessary?: |# :load-toplevel :execute)
  (defparameter *stream-methods* (make-hash-table)))

(defun check-open (method stream)
  (unless (%open-stream-p stream)
    (error (make-condition simple-stream-error
                           :stream stream
                           :format-control "~S on ~S is illegal"
                           :format-arguments (list method stream)))))

(defmacro define-stream-methods (class-name &rest methods)
  `(progn
     ,@(mapcar (lambda (method)
                 (let ((minfo (gethash (first method) *stream-methods*)))
                   (unless minfo
                     (error "Unknown method ~S; please use DEFINE-FORWARD first"
                            (first method)))
                   (destructuring-bind (name lambda-list stream-name check-open-p)
                       minfo
                     `(defmethod ,name 
                          ,(make-method-lambda-list lambda-list stream-name class-name)
                        (check-open ',name ,stream-name)
                        ,@(rest method)))))
               methods)))


(defmacro define-forward (name arguments &body body)
  (let* ((documentation     (extract-documentation body))
         (declarations      (declarations-hash-table (extract-declarations  body)))
         (body              (extract-body          body))
         (stream-argument   (caar  (gethash 'stream-argument   declarations)))
         (stream-designator (caar  (gethash 'stream-designator declarations)))
         (stream-name       (or stream-argument  
                                (if (consp stream-designator)
                                    (first stream-designator)
                                    stream-designator)))
         (check-stream-type (caar  (gethash 'check-stream-type declarations)))
         (cl-forward        (caar  (gethash 'cl-forward        declarations)))
         (check-open-p      (caar  (gethash 'check-open-p      declarations)))
         (lambda-list       (parse-lambda-list arguments :ordinary))
         (m-name            (intern (format nil "%~A" name)))
         (cl-name           (intern (string name) "COMMON-LISP")))
    (setf (gethash name *stream-methods*) 
          (list m-name lambda-list stream-name check-open-p))
    `(progn
       (defun ,name ,arguments
         ,@ (when documentation (list documentation))
         ,@ (when stream-designator
              `((setf ,stream-name (stream-designator 
                                    ,stream-name
                                    ,(if (listp stream-designator)
                                         (ecase (second stream-designator)
                                           ((:input)  '*standard-input*)
                                           ((:output) '*standard-output*))
                                         '*standard-input*)))))
         ,(if (lambda-list-rest-p lambda-list)
              `(apply (function ,m-name) ,@(make-argument-list lambda-list))
              `(,m-name         ,@(butlast (make-argument-list lambda-list)))))
       ,@ (when cl-forward
            `((defmethod ,m-name 
                  ,(make-method-lambda-list lambda-list stream-name 'cl-stream)
                ,(let ((arguments (mapcar
                                   (lambda (arg)
                                     (if (eq arg stream-name)
                                         `(cl-stream-stream ,stream-name)
                                         arg))
                                   (make-argument-list lambda-list))))
                      (if (lambda-list-rest-p lambda-list)
                          `(apply (function ,cl-name) ,@arguments)
                          `(,cl-name ,@(butlast arguments)))))))
       ,@ (when check-stream-type
            `((defmethod ,m-name ,(make-method-lambda-list lambda-list stream-name 't)
                (raise-type-error ,stream-name ',check-stream-type))))
       ,@ (mapcar
           (lambda (method)
             (when (and (listp method) (eq :method (car method)))
               (destructuring-bind (method class-name &body body) method
                 (declare (ignore method))
                 `(defmethod ,m-name
                      ,(make-method-lambda-list lambda-list stream-name class-name)
                    ,@body))))
           body))))


(define-forward input-stream-p       (stream) 
  (declare (stream-argument stream)
           (check-stream-type stream)
           (cl-forward t)))

(define-forward output-stream-p      (stream) 
  (declare (stream-argument stream)
           (check-stream-type stream)
           (cl-forward t)))

(define-forward interactive-stream-p (stream) 
  (declare (stream-argument stream)
           (check-stream-type stream)
           (cl-forward t))
  (:method stream nil))

(define-forward open-stream-p        (stream) 
  (declare (stream-argument stream)
           (check-stream-type stream)
           (cl-forward t)))

(define-forward stream-element-type  (stream) 
  (declare (stream-argument stream)
           (check-stream-type stream)
           (cl-forward t)))

(defun streamp (object) (typep object 'stream))

(defun eof-stream (stream eof-error-p eof-value)
  (if eof-error-p (error (make-condition 'eof-error :stream stream)) eof-value))



(define-forward read-byte (stream &optional (eof-error-p t) (eof-value nil))
  (declare (stream-argument stream)
           (check-stream-type stream) 
           (cl-forward t) (check-open-p t))
  (:method echo-stream
    (let ((byte (read-byte (%echo-stream-input-stream stream) nil stream)))
      (if (eq byte stream)
          (eof-error stream eof-error-p eof-value)
          (progn 
            (write-byte byte  (%echo-stream-output-stream stream))
            byte))))
  (:method file-stream
    )
  (:method string-input-stream
    (if (< (%string-stream-index stream)
           (or (%string-stream-end stream) 
               (length (%string-stream-input-string stream))))
        (char-code (aref (%string-stream-input-string stream)
                         (prog1 (%string-stream-index stream)
                           (incf (%string-stream-index stream)))))
        (eof-error stream eof-error-p eof-value)))
  (:method synonym-stream
    (read-byte (symbol-value (%synonym-stream-symbol stream))
               eof-error-p eof-value))
  (:method two-way-stream
    (read-byte (%two-way-stream-input-stream stream)
               eof-error-p eof-value)))

(define-forward write-byte (byte stream)
  (declare (stream-argument stream)
           (check-stream-type stream) 
           (cl-forward t) (check-open-p t))
  (:method broadcast-stream
    (broadcast-stream-operation (ostream stream) (write-byte byte ostream))
    byte)
  (:method echo-stream
    (write-byte byte  (%echo-stream-output-stream stream)))
  (:method file-stream
    )
  (:method string-output-stream
    (vector-push-extend (char-code byte) (%string-stream-output-string stream))
    byte)
  (:method synonym-stream
    (write-byte (symbol-value (%synonym-stream-symbol stream))))
  (:method two-way-stream
    (write-byte (%two-way-stream-output-stream stream))))

(define-forward peek-char (&optional (peek-type nil) (stream *standard-input*)
                                     (eof-error-p t) (eof-value nil)
                                     (recursive-p nil))
  (declare (stream-designator (stream :input))
           (cl-forward t) (check-open-p t)))


(define-forward read-char (&optional (input-stream *standard-input*) 
                                     (eof-error-p t) (eof-value nil)
                                     (recursive-p nil))
  (declare (stream-designator (input-stream :input))
           (cl-forward t) (check-open-p t))
  (:method echo-stream
    (let ((char (read-char (%echo-stream-input-stream stream) nil stream)))
      (if (eq char stream)
          (eof-error stream eof-error-p eof-value)
          (progn 
            (write-char char (%echo-stream-output-stream stream))
            char))))
  (:method file-stream
    )
  (:method string-input-stream
    (if (< (%string-stream-index stream)
           (or (%string-stream-end stream) 
               (length (%string-stream-input-string stream))))
        (aref (%string-stream-input-string stream)
              (prog1 (%string-stream-index stream)
                (incf (%string-stream-index stream))))
        (eof-error stream eof-error-p eof-value)))
  (:method synonym-stream
    (read-char (symbol-value (%synonym-stream-symbol stream))
               eof-error-p eof-value))
  (:method two-way-stream
    (read-char (%two-way-stream-input-stream stream)
               eof-error-p eof-value)))


(define-forward read-char-no-hang (&optional (input-stream *standard-input*) 
                                             (eof-error-p t) (eof-value nil)
                                             (recursive-p nil))
  (declare (stream-designator (input-stream :input))
           (cl-forward t) (check-open-p t)))

(define-forward terpri (&optional (output-stream *standard-output*))
  (declare (stream-designator (output-stream :output))
           (cl-forward t) (check-open-p t)))

(define-forward fresh-line (&optional (output-stream *standard-output*))
  (declare (stream-designator (output-stream :output))
           (cl-forward t) (check-open-p t)))

(define-forward unread-char (character &optional (input-stream *standard-input*))
  (declare (stream-designator (input-stream :input))
           (cl-forward t) (check-open-p t)))

(define-forward write-char (character
                            &optional (output-stream *standard-output*))
  (declare (stream-designator (output-stream :output))
           (cl-forward t) (check-open-p t))
  (:method echo-stream
    (write-char character (%echo-stream-output-stream stream)))
  (:method file-stream
    )
  (:method string-output-stream
    (vector-push-extend character (%string-stream-output-string stream))
    character)
  (:method synonym-stream
    (write-char (symbol-value (%synonym-stream-symbol stream))))
  (:method two-way-stream
    (write-char (%two-way-stream-output-stream stream))))

(define-forward read-line (&optional (input-stream *standard-input*)
                                     (eof-error-p t) (eof-value nil)
                                     (recursive-p nil))
  (declare (stream-designator (input-stream :input))
           (cl-forward t) (check-open-p t)))
  
(define-forward write-string (string
                              &optional (output-stream *standard-output*)
                              &key (start 0) (end nil))
  (declare (stream-designator (output-stream :output))
           (cl-forward t) (check-open-p t)))

(define-forward write-line (string
                            &optional (output-stream *standard-output*)
                            &key (start 0) (end nil))
  (declare (stream-designator (output-stream :output))
           (cl-forward t) (check-open-p t)))

(define-forward read-sequence (sequence stream &key (start 0) (end nil))
  (declare (stream-argument stream)
           (cl-forward t) (check-open-p t)))

(define-forward write-sequence (sequence stream &key (start 0) (end nil))
  (declare (stream-argument stream)
           (cl-forward t) (check-open-p t)))

(define-forward file-length (stream)
  (declare (stream-argument stream)
           (check-stream-type file-stream) 
           (cl-forward t))
  (:method stream (error "not implemented yet")))

(define-forward file-position (stream 
                               &optional (position-spec nil position-spec-p))
  (declare (stream-argument stream)
           (cl-forward t) (check-open-p t)))

(define-forward file-string-length (stream object)
  (declare (stream-argument stream)
           (check-stream-type file-stream) 
           (cl-forward t) (check-open-p t)))



(defun open (filespec &key (direction :input)
             (element-type 'character)
             (if-exists nil if-exists-p)
             (if-does-not-exist nil if-does-not-exist-p)
             (external-format :default))
  (error "Not implemented yet")
  (ecase direction 
    ((:probe) )
    ((:input) )
    ((:io) )
    ((:output) ))

  ;; filespec  ¬N.T     ¬N.T.3<NEWEST ¬N.T.3>NEWEST  N.T.3=NEWEST  N.T.3<NEWEST
  ;;   N.T     no exist     newest        newest       newest         newest
  ;;   N.T.3   no exist     no exist      no exist     newest      old version
  ;;                     create newest  create newest
  :input :probe
  (ecase if-does-not-exist
    ((:error)             (error "..."))
    ((:create)            (create-new-version newest))
    ((nil)                (return-from open nil)))

  :output :io
  (ecase if-exists
    ((:error)             (error "..."))
    ((:new-version)       (create-new-version "..."))
    ((:rename)            (rename-file "..." "N.TYPE-OLD-###")
     (create-file-at-path "..."))
    ((:rename-and-delete) (rename-file "..." "N.TYPE-OLD-###")
     (create-file-at-path "...")
     (delete-file "N.TYPE-OLD-###"))
    ((:overwrite)         (if newest
                              (file-entry "...")
                              (copy-new-version "...")))
    ((:append)            (if newest
                              (file-entry "...")
                              (copy-new-version "...")))
    ((:supersede)         (create-new-version "..."))
    ((nil)                (return-from open nil)))


  :element-type :external-format
  if new file then
  set element-type
  if file already existed then 
  check element-type match
  :default ==> get element-type

  (make-instance 'file-stream
    :open-p  (and (not (eq direction :probe)) |_...|)
    :element-type element-type
    :external-format
    #+clisp (if (eq external-format :default)
                                 custom:*default-file-encoding*
                                 external-format)
    #-clisp external-format
    :input-p  (member direction '(:input :io))
    :output-p (member direction '(:output :io))
    :pathname filespec                  ; or (pathname filespec) ?
    :file (file-entry (truename filespec))))

(define-forward stream-external-format (stream)
  (declare (stream-argument stream)
           (check-stream-type stream) 
           (cl-forward t)))

(define-forward close (stream &key (abort nil))
  (declare (stream-argument stream)
           (check-stream-type stream) 
           (cl-forward t)))

(define-forward listen (&optional input-stream)
  (declare (stream-designator input-stream)
           (cl-forward t) (check-open-p t)))

(define-forward clear-input (&optional input-stream)
  (declare (stream-designator (input-stream :input))
           (cl-forward t) (check-open-p t)))

(define-forward clear-output (&optional output-stream)
  (declare (stream-designator (output-stream :output))
           (cl-forward t) (check-open-p t)))

(define-forward force-output (&optional output-stream)
  (declare (stream-designator (output-stream :output))
           (cl-forward t) (check-open-p t)))

(define-forward finish-output (&optional output-stream)
  (declare (stream-designator (output-stream :output))
           (cl-forward t) (check-open-p t)))


(defun y-or-n-p (&optional format-string &rest args)
  (when format-string
    (fresh-line *query-io*)
    (apply (function format) *query-io* format-string args)
    (write-string " (y/n) " *query-io*))
  (loop
     (let ((line (string-left-trim " " (read-line *query-io*))))
       (when (plusp (length line))
         (let ((first-char (char-upcase (char line 0))))
           (when (char-equal first-char #\n) (return nil))
           (when (char-equal first-char #\y) (return t))))
       (write-string "Please answer with y or n : " *query-io*))))


(defun yes-or-no-p (&optional format-string &rest args)
  (when format-string
    (fresh-line *query-io*)
    (apply (function format) *query-io* format-string args)
    (write-string " (yes/no) " *query-io*))
  (loop
     (clear-input *query-io*)
     (let ((line (string-trim " " (read-line *query-io*))))
       (when (string-equal line "NO")  (return nil))
       (when (string-equal line "YES") (return t)))
     (write-string "Please answer with yes or no : " *query-io*)))


(defun make-synonym-stream (symbol)
  (check-type symbol symbol)
  (make-instance 'synonym-stream :symbol symbol))

(define-forward synonym-stream-symbol (synonym-stream)
  (declare (stream-argument synonym-stream)
           (check-stream-type synonym-stream)))

(defun make-broadcast-stream (&rest streams)
  (dolist (stream streams)
    (unless (output-stream-p stream)
      (error (make-condition
              'simple-type-error 
              :datum stream
              :expected-type 'stream
              :format-control "Stream is not an output stream"))))
  (make-instance 'broadcast-stream :streams streams))

(define-forward broadcast-stream-streams (broadcast-stream)
  (declare (stream-argument broadcast-stream)
           (check-stream-type broadcast-stream)))

(defmacro broadcast-stream-operation ((output-stream broadcast-stream)
                                      &body body)
  `(let ((results '()))
     (dolist (,output-stream (%broadcast-stream-streams ,broadcast-stream)
              (values-list results))
       (setf results (multiple-value-list (progn ,@body))))))

(define-stream-methods broadcast-stream
    (write-byte     (do-broadcast (ostream stream)
                      (write-char byte ostream))
                    byte)
  (write-char     (do-broadcast (ostream stream)
                    (write-char character ostream))
                  character)
  (terpri         (do-broadcast (ostream stream) 
                    (terpri ostream))
                  nil)
  (fresh-line     (do-broadcast (ostream stream) 
                    (fresh-line ostream)))
  (write-string   (do-broadcast (ostream stream) 
                    (write-string string ostream :start start :end end))
                  string)
  (write-line     (do-broadcast (ostream stream) 
                    (write-line string ostream :start start :end end))
                  string)
  (write-sequence (do-broadcast (ostream stream) 
                    (write-sequence sequence ostream :start start :end end))
                  sequence)
  (clear-output   (do-broadcast (ostream stream) 
                    (clear-output ostream)))
  (force-output   (do-broadcast (ostream stream) 
                    (force-output ostream)))
  (finish-output  (do-broadcast (ostream stream) 
                    (finish-output ostream)))
  (file-length             (if (%broadcast-stream-streams stream)
                               (file-length 
                                (car (last (%broadcast-stream-streams stream))))
                               0))
  (file-position           (if (%broadcast-stream-streams stream)
                               (file-position
                                (car (last (%broadcast-stream-streams stream))))
                               0))
  (file-string-length      (if (%broadcast-stream-streams stream)
                               (file-string-length
                                (car (last (%broadcast-stream-streams stream))))
                               1))
  (stream-external-format  (if (%broadcast-stream-streams stream)
                               (stream-external-format
                                (car (last (%broadcast-stream-streams stream))))
                               't))
  (close             (prog1 (%open-p stream)
                       (setf (%open-p stream) nil
                             (%broadcast-stream-streams stream) nil))))


(defun make-two-way-stream (input-stream output-stream)
  (unless (input-stream-p stream)
    (error (make-condition
            'simple-type-error 
            :datum input-stream
            :expected-type 'stream
            :format-control "Stream is not an input stream")))
  (unless (output-stream-p stream)
    (error (make-condition
            'simple-type-error 
            :datum output-stream
            :expected-type 'stream
            :format-control "Stream is not an output stream")))
  (make-instance 'two-way-stream
    :input-stream input-stream
    :output-stream output-stream))

(define-forward two-way-stream-input-stream (two-way-stream)
  (declare (stream-argument two-way-stream)
           (check-stream-type two-way-stream)))

(define-forward two-way-stream-output-stream (two-way-stream)
  (declare (stream-argument two-way-stream)
           (check-stream-type two-way-stream)))

(defun make-echo-stream (input-stream output-stream)
  (unless (input-stream-p stream)
    (error (make-condition
            'simple-type-error 
            :datum input-stream
            :expected-type 'stream
            :format-control "Stream is not an input stream")))
  (unless (output-stream-p stream)
    (error (make-condition
            'simple-type-error 
            :datum output-stream
            :expected-type 'stream
            :format-control "Stream is not an output stream")))
  (make-instance 'echo-stream
    :input-stream input-stream
    :output-stream output-stream))

(define-forward echo-stream-input-stream (echo-stream)
  (declare (stream-argument echo-stream)
           (check-stream-type echo-stream)))

(define-forward echo-stream-output-stream (echo-stream)
  (declare (stream-argument echo-stream)
           (check-stream-type echo-stream)))

(define-stream-methods echo-stream
    (read-byte)
  (read-char)
  (read-char-no-hang)
  (peek-char)
  (unread-char)
  (read-line)
  (read-sequence)
  (terpri)
  (fresh-line)
  (write-byte)
  (write-char)
  (write-string)
  (write-line)
  (write-sequence)
  (listen)
  (clear-input)
  (clear-output)
  (force-output)
  (finish-output)

  (file-length)
  (file-position)
  (file-string-length)
  (stream-external-format)
  (close))

(defun make-concatenated-stream (&rest input-streams)
  (dolist (stream streams)
    (unless (input-stream-p stream)
      (error (make-condition
              'simple-type-error 
              :datum stream
              :expected-type 'stream
              :format-control "Stream is not an input stream"))))
  (make-instance 'concatenated-stream :streams input-streams))

(define-forward concatenated-stream-streams (concatenated-stream)
  (declare (stream-argument concatenated-stream)
           (check-stream-type concatenated-stream)))

(defun !concatenated-read-element (read-element 
                                   stream eof-error-p eof-value recursive-p)
  (let ((current (first (%concatenated-stream-streams stream))))
    (if (null current)
        (eof-error stream eof-error-p eof-value)
        (let ((element (multiple-value-list 
                        (funcall read-element current nil stream recursive-p))))
          (cond
            ((eq (car element) stream)
             (pop (%concatenated-stream-streams stream))
             (!concatenated-read-element 
              read-element stream eof-error-p eof-value recursive-p))
            ((second element)
             (pop (%concatenated-stream-streams stream))
             (multiple-value-bind (line missing-newline-p)
                 (!concatenated-read-element 
                  read-element stream eof-error-p eof-value recursive-p)
               (values (concatenate 'string (first element) line)
                       missing-newline-p)))
            (t (values-list element)))))))

(define-stream-methods concatenated-stream
    (read-byte         (!concatenated-read-element 
                        (lambda (s e v r) (declare (ignore r)) (read-byte s e v))
                        stream eof-error-p eof-value nil))
  (read-char         (!concatenated-read-element 
                      (function read-char)
                      stream eof-error-p eof-value recursive-p))
  (read-char-no-hang (!concatenated-read-element
                      (function read-char-no-hang)
                      stream eof-error-p eof-value recursive-p))
  (peek-char         (!concatenated-read-element
                      (lambda (s e v r) (peek-char peek-type s e v r))
                      stream eof-error-p eof-value recursive-p))
  (unread-char
   (let ((current (first (%concatenated-stream-streams stream))))
     (if (null current)
         (push (make-string-input-stream (string character))
               (%concatenated-stream-streams stream))
         (unread-char character current))))
  (read-line         (!concatenated-read-element 
                      (lambda (s e v r) (declare (ignore r)) (read-line s e v))
                      stream eof-error-p eof-value recursive-p))
  (read-sequence
   (let ((current (first (%concatenated-stream-streams stream))))
     (if (null current)
         (eof-error stream eof-error-p eof-value)
         (let* ((end      (or end (length sequence)))
                (position (read-stream sequence current start end)))
           (if (< position end)
               (progn
                 (pop (%concatenated-stream-streams stream))
                 (setf current (first (%concatenated-stream-streams stream)))
                 (if (null current)
                     position
                     (read-sequence sequence stream :start position :end end)))
               position)))))
  (listen
   (let ((current (first (%concatenated-stream-streams stream))))
     (warn "LISTEN may return NIL in the middle of a concatenated-stream when we're at the end of one of the substreams")
     (listen current)))
  (clear-input
   (let ((current (first (%concatenated-stream-streams stream))))
     (and current (clear-input current))))
  (stream-external-format ;; or use the attribute?
   (let ((current (first (%concatenated-stream-streams stream))))
     (if current
         (stream-external-format current)
         :default)))
  (close
   (prog1 (%open-p stream)
     (setf (%open-p stream) nil
           (%concatenated-stream-streams stream) nil))))

#||
(define-stream-methods input-stream
    (read-byte)
  (read-char)
  (read-char-no-hang)
  (peek-char)
  (unread-char)
  (read-line)
  (read-sequence)
  (listen)
  (clear-input)

  (file-length)
  (file-position)
  (file-string-length)
  (stream-external-format)
  (close))

(define-stream-methods output-stream
    (write-byte)
  (write-char)
  (terpri)
  (fresh-line)
  (write-string)
  (write-line)
  (write-sequence)
  (listen)
  (clear-output)
  (force-output)
  (finish-output)

  (file-length)
  (file-position)
  (file-string-length)
  (stream-external-format)
  (close))

(define-stream-methods io-stream
    (read-byte)
  (read-char)
  (read-char-no-hang)
  (peek-char)
  (unread-char)
  (read-line)
  (read-sequence)
  (terpri)
  (fresh-line)
  (write-byte)
  (write-char)
  (write-string)
  (write-line)
  (write-sequence)
  (listen)
  (clear-input)
  (clear-output)
  (force-output)
  (finish-output)

  (file-length)
  (file-position)
  (file-string-length)
  (stream-external-format)
  (close))
||#

(defun make-string-input-stream (string &optional (start 0) (end nil))
  (make-instance 'string-input-stream
    :string string
    :start start
    :end end))

(defun make-string-output-stream (&key (element-type 'character element-type-p))
  (make-instance 'string-output-stream
    :string (make-array 8 :fill-pointer 0 :adjustable t
                        :element-type element-type)))

(define-forward get-output-stream-string (string-output-stream)
  (declare (stream-argument   string-output-stream)
           (check-stream-type string-output-stream))
  (:method string-output-stream
    (%string-stream-output-string string-output-stream)))


;; Macros are taken from clisp sources, and adpated.
(eval-when (:execute :compile-toplevel :load-toplevel)
 (defun parse-body (body)
   (values (extract-body body) 
           (let ((decls '()))
             (maphash 
              (lambda (k v) 
                (setf decls (nconc (mapcar (lambda (d) (cons k v)) v) decls)))
              (declarations-hash-table (extract-declarations body)))
             decls))))
 
(defmacro with-open-file ((stream &rest options) &body body)
  (multiple-value-bind (body-rest declarations)  (parse-body body)
    `(let ((,stream (open ,@options)))
       (declare (read-only ,stream) ,@declarations)
       (unwind-protect
            (multiple-value-prog1 (progn ,@body-rest)
              (when ,stream (close ,stream)))
         (when ,stream (close ,stream :abort t))))))

(defmacro with-open-stream ((var stream) &body body)
  (multiple-value-bind (body-rest declarations) (parse-body body)
    `(let ((,var ,stream))
       (declare (read-only ,var) ,@declarations)
       (unwind-protect
            (multiple-value-prog1 (progn ,@body-rest) (close ,var))
         (close ,var :abort t)))))

(defmacro with-input-from-string ((var string  &key (index nil sindex) 
                                       (start '0 sstart) (end 'nil send))
                                  &body body)
  (multiple-value-bind (body-rest declarations) (parse-body body)
    `(let ((,var (make-string-input-stream 
                  ,string
                  ,@(if (or sstart send)
                        `(,start ,@(if send `(,end) '()))
                        '()))))
       (declare (read-only ,var) ,@declarations)
       (unwind-protect
            (progn ,@body-rest)
         ,@(when sindex `((setf ,index (%string-stream-index ,var))))
         (close ,var)))))

(defmacro with-output-to-string ((var &optional (string nil)
                                      &key (element-type ''character))
                                 &body body)
  (multiple-value-bind (body-rest declarations) (parse-body body)
    (if string
        (let ((ignored-var (gensym)))
          `(let ((,var (make-instance 'string-output-stream :string ,string))
                 (,ignored-var ,element-type))
             (declare (read-only ,var) (ignore ,ignored-var) ,@declarations)
             (unwind-protect
                  (progn ,@body-rest)
               (close ,var))))
        `(let ((,var (make-string-output-stream :element-type ,element-type)))
           (declare (read-only ,var) ,@declarations)
           (unwind-protect
                (progn ,@body-rest (get-output-stream-string ,var))
             (close ,var))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; the END ;;;;
