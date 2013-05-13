(in-package :cl-user)

(defpackage :asdf-viewer
  (:use :cl :asdf :capi)
  (:shadowing-import-from :asdf #:component-name)
  (:import-from :asdf #:defsystem-depends-on #:system-defsystem-depends-on #:missing-requires #:coerce-name)
  (:export #:define-lisp-world-viewer-and-show)
  )

(in-package :asdf-viewer)

(defparameter *lisp-world-viewer-window* nil) 

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

(defun sys-reverse-deps (sys &key (reverse-deps-hash *reverse-deps-hash*))
  (gethash (find-system-or-not-found sys) reverse-deps-hash))

(let ((ht (make-hash-table)))
  (fill-reverse-deps-hash :reverse-deps-hash ht)
  ht)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun system-as-keyword (system-designator)
  (intern (string-upcase (coerce-name system-designator)) :keyword))

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

(defun show-system-callback (data interface)
  (declare (ignore interface))
  (go-to-definition data)
  )

(defun get-dependencies-roots ()
  (sort (get-all-systems) #'string-lessp))

(defun get-required-for-roots ()
  (progn
    (fill-reverse-deps-hash)
    (sort (get-all-systems :as-systems t) #'string-lessp :key #'coerce-name)))

(defun test-callback (&rest args)
  (display-message "~S" args))

(defun get-item-position (element tree)
  (position (coerce-name element) (tree-view-roots tree) :test #'string-equal :key #'coerce-name))

(defun gen-callback-select-also (tree-pane1 tree-pane2 &aux tree1 tree2)
  (lambda (element interface &aux cur-desc sys)
;;;;;;;; Show details ;;;;;;;;;
    (setf sys (find-system (string-downcase (coerce-name element))))
    (setf cur-desc (handler-case (system-description (find-system sys))
                     (unbound-slot () "")))
    (setf (display-pane-text (slot-value interface 'details-description)) cur-desc)
    (setf cur-desc (handler-case (system-long-description (find-system sys))
                     (unbound-slot () "")))
    (setf (display-pane-text (slot-value interface 'details-long-description)) cur-desc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setf tree1 (slot-value interface tree-pane1))
    (setf tree2 (slot-value interface tree-pane2))
    (setf (choice-selection tree2) (get-item-position (choice-selected-item tree1) tree1))
    ))

(progn
  (defun define-lisp-world-viewer-and-show ()
    (define-interface lisp-world-viewer ()
      ()
      (:panes
       (sys-deps tree-view
                 :title "ASDF-systems dependencies"
                 :roots (get-dependencies-roots)
                 :children-function 'sys-deps
                 :action-callback #'show-system-callback
                 :selection-callback (gen-callback-select-also 'sys-deps 'sys-req-for)
                 )
       (sys-req-for tree-view
                    :title "ASDF-systems required for"
                    :roots (get-required-for-roots)
                    :print-function #'coerce-name
                    :children-function 'sys-reverse-deps 
                    :action-callback #'show-system-callback
                    :selection-callback (gen-callback-select-also 'sys-req-for 'sys-deps)
                    )
       (details-description display-pane
                            :title "Description:"
                            :title-position :left
                            )
       (details-long-description display-pane
                            :title "Long description:"
                            :title-position :left
                            )

       )
      (:layouts
       (col-deps-details column-layout '(row-asdf-deps details-layout))
       (row-asdf-deps row-layout '(sys-deps sys-req-for))
       (details-layout column-layout '(details-description details-long-description))

       )
      (:default-initargs :title "ASDF-systems"
       :visible-min-width 800
       :visible-min-height 600
       ))
    (display (setf *lisp-world-viewer-window* (make-instance 'lisp-world-viewer))))
  ;(define-lisp-world-viewer-and-show)
  )





   