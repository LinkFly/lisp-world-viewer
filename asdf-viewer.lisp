(in-package :cl-user)

(defpackage :asdf-viewer
  (:use :cl :asdf :capi)
  (:shadowing-import-from :asdf #:component-name)
  (:import-from :asdf #:defsystem-depends-on #:system-defsystem-depends-on))

(in-package :asdf-viewer)

(defmethod system-depends-on ((sys system))
  (mapcar #'coerce-name (rest (second (component-depends-on 'load-op sys)))))

(defmethod system-depends-on ((sys symbol))
  (system-depends-on (find-system (coerce-name sys))))

(defun sys-deps (sys)
  (setf sys (handler-case (find-system sys)
              (missing-component () nil)))
  (unless sys (return-from sys-deps))
  (append (when (slot-boundp sys 'defsystem-depends-on)
            (mapcar (lambda (sys)
                      (string-upcase (component-name sys)))
                    (system-defsystem-depends-on (find-system sys))))
          (system-depends-on sys)))

(defun node-children (node)
  (get-deps node))

(defun get-all-systems ()
  (let (systems) 
    (map-systems (lambda (sys) (push sys systems)))
    (mapcar #'component-name (nreverse systems))))

(contain 
 (make-instance 'tree-view
                :roots (get-all-systems)
                :children-function 'sys-deps
                ))

#|(contain 
 (make-instance 'tree-view
                :roots (mapcar #'package-name (list-all-packages))
                :children-function 'sys-deps (mapcar #'package-name (package-use-list (find-package "RESTAS")))
                (mapcar #'package-name (package-used-by-list (find-package "ITERATE")))
                ))
|#

