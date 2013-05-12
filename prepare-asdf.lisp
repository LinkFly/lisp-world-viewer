(in-package :asdf)

(defparameter *prepare-asdf-file-loaded* nil)

(defun system-as-keyword (system-designator)
  (intern (string-upcase (asdf:coerce-name system-designator)) :keyword))

(setf (macro-function 'old-defsystem) (macro-function 'asdf:defsystem))

(defmacro new-defsystem (name &body options)
  (setf name (system-as-keyword name))
  `(dspec:def (asdf:defsystem ,name)
     (when (lispworks:record-definition
            `(asdf:defsystem ,',name)
            (dspec:location))
       (prog1 (old-defsystem ,name ,@options)
         (setf (get ',name 'asdf-system)
               (asdf:find-system ',name)
               )))))

(setf (macro-function 'asdf:defsystem) (macro-function 'new-defsystem))

(defun find-asdf-system (name)
  (get name 'asdf-system))

(dspec:define-dspec-class asdf:defsystem nil
  "Defined saved value"
  :definedp #'(lambda (name) (not (null (find-asdf-system name))))
  :undefiner #'(lambda (dspec) `(remprop ,(dspec:dspec-name dspec) 'asdf-system))
  :object-dspec #'(lambda (obj) (and (typep obj 'system)
                                     `(asdf:defsystem ,(system-as-keyword obj)))))

(dspec:define-form-parser asdf:defsystem (name &rest body)
  `(asdf:defsystem ,(system-as-keyword name)))

#| Check: 
(asdf:defsystem :my-name6 :components ((:file "exp")))
(asdf:defsystem #:my-name7 :components ((:file "exp")))
(asdf:defsystem my-name8 :components ((:file "exp")))
(asdf:defsystem "my-name9" :components ((:file "exp")))

:my-name6
:my-name7
:my-name8
:my-name9
|#

;;;;;;;;; Redefining (for finding) this ASDF-system ;;;;;;;;
(unless *prepare-asdf-file-loaded*
  (setf *prepare-asdf-file-loaded* t)
  (load (asdf:system-source-file :prepare-asdf)))
;;;;;;;;;;;;;;;;;;;;;;;;;

