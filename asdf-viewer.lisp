(in-package :cl-user)

(defpackage :asdf-viewer
  (:use :cl :asdf :capi)
  (:shadowing-import-from :asdf #:component-name)
  (:import-from :asdf #:defsystem-depends-on #:system-defsystem-depends-on #:missing-requires))

(in-package :asdf-viewer)

(defparameter *reverse-deps-hash* (make-hash-table)) ;;filling of fill-reverse-deps-hash function

(defstruct not-found
  (system nil :type string))

(defun find-system-or-not-found (sys)
  (handler-case (find-system sys)
    (missing-component (condition) (make-not-found :system (missing-requires condition)))))

(defmethod system-depends-on ((sys system) &key as-systems defsystem-depends-on &aux result)
  (setf result (if (and defsystem-depends-on (slot-boundp sys 'defsystem-depends-on))
                   (system-defsystem-depends-on sys)
                 (rest (second (component-depends-on 'load-op sys)))))
  (setf result (reverse result))
  (if as-systems
    (mapcar #'find-system-or-not-found result)
    (mapcar #'coerce-name result)))

(defmethod system-depends-on ((not-found not-found) &key as-systems defsystem-depends-on &aux result)
  (declare (ignore not-found as-systems defsystem-depends-on result))
  nil)

(defmethod system-depends-on ((sys t) &key as-systems defsystem-depends-on)
  (system-depends-on (find-system (coerce-name sys)) :as-systems as-systems :defsystem-depends-on defsystem-depends-on))

(defun sys-deps (sys)
  (setf sys (handler-case (find-system sys)
              (missing-component () nil)))
  (unless sys (return-from sys-deps))
  (append (when (slot-boundp sys 'defsystem-depends-on)
            (mapcar #'string-upcase
                    (system-depends-on (find-system sys) :defsystem-depends-on t)))
          (system-depends-on sys)))

(defun get-all-systems (&key as-systems)
  (let (systems) 
    (map-systems (lambda (sys) (push sys systems)))
    (setf systems (reverse systems))
    (when as-systems (return-from get-all-systems systems))
    (mapcar #'component-name (nreverse systems))))

(defun fill-reverse-deps-hash (&key (reverse-deps-hash *reverse-deps-hash*) &aux all-systems)
  (setf all-systems (get-all-systems :as-systems t))
  (dolist (sys all-systems)
    (setf (gethash sys reverse-deps-hash) nil))
  (dolist (sys all-systems)
    ;(print (list 'outer sys))
    (dolist (dep (append (system-depends-on sys :as-systems t :defsystem-depends-on t)
                         (system-depends-on sys :as-systems t :defsystem-depends-on nil)))
      ;(print (list 'inner (list sys dep)))
      (setf (gethash dep reverse-deps-hash) (let ((cur-reverse-deps (gethash dep reverse-deps-hash)))
                               (if (member sys cur-reverse-deps)
                                   cur-reverse-deps
                                 (append cur-reverse-deps (list sys))))))))

;(system-depends-on :swank :as-systems t)

;(defun hash-table-keys (ht)
;  (loop :for key :being :the :hash-key :in ht :collect key))

(defun sys-reverse-deps (sys &key (reverse-deps-hash *reverse-deps-hash*))
  (gethash (find-system-or-not-found sys) reverse-deps-hash))

(let ((ht (make-hash-table)))
  (fill-reverse-deps-hash :reverse-deps-hash ht)
  ht)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun system-as-keyword (system-designator)
  (intern (string-upcase (asdf:coerce-name system-designator)) :keyword))

#|(defun go-to-definition (sys)
;(capi:apply-in-pane-process ed 'capi:call-editor ed 'editor:forward-character-command)
 (editor:find-source-command
  (capi:find-interface 'lispworks-tools:editor)
  (system-as-keyword sys)))
|#

(defun go-to-definition (sys &aux ed)
;(capi:apply-in-pane-process ed 'capi:call-editor ed 'editor:forward-character-command)
  (setf ed (capi:find-interface 'lispworks-tools:editor))
  (capi:apply-in-pane-process ed
                              'capi:call-editor
                              ed
                              (list 'editor:find-source-command ed (system-as-keyword sys)))
  (capi:apply-in-pane-process ed 'editor:hl-off-command))
;(go-to-definition :alexandria)
;;;;;;;;;;;;;;;;;;;;;;

;(defun open-asd-file (sys)
;  (goto-or-create-xy (system-source-file sys) 0 0))

(defun show-system-callback (data interface)
  (declare (ignore interface))
;  (open-asd-file data)
  (go-to-definition data)
  )

(contain 
 (make-instance 'tree-view
                :title "ASDF-systems and their dependencies"
                :roots (sort (get-all-systems) #'string-lessp)
                :children-function 'sys-deps
                :action-callback #'show-system-callback
                ))

(fill-reverse-deps-hash)

(contain 
 (make-instance 'tree-view
                :title "ASDF-systems depends on"
                :roots (sort (get-all-systems :as-systems t) #'string-lessp :key #'asdf:coerce-name)
                :print-function #'coerce-name
                :children-function 'sys-reverse-deps 
                :action-callback #'show-system-callback
                ))
