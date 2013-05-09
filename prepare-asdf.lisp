(in-package :cl-user)
(defparameter +this-sys+ :prepare-asdf)
(defparameter *file-loaded* nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; Preparing code ;;;;;;;;;;;;;;;;;;;;;;
(defmacro empty-keyword-fn (keyword &optional (closure-value nil closure-value-p))
  (the keyword keyword)
  `(handler-bind ((simple-error (lambda (condition) 
                                    (let ((restart (find-restart 'continue condition)))
                                      (when restart (invoke-restart restart))))))
       ,(if closure-value-p
            `(progn 
               (defun ,keyword () ,closure-value)
               (,keyword))
          `(defun ,keyword () 
             (empty-keyword-fn ,keyword (asdf:find-system ,keyword))
             ))))

;(macroexpand-1 '(empty-keyword-fn :asdf))
;(empty-keyword-fn :asdf)
;(:asdf)
;(symbol-function :asdf)


(defun system-as-keyword (system-designator)
  (intern (string-upcase (asdf:coerce-name system-designator)) :keyword))

;;;;;;;;;;;;;;;;;;;;;; Test code ;;;;;;;;;;;;;;;;;;
#| 
(defmacro my-defsystem4 (name &rest body)
  (setf name (system-as-keyword name))
  `(asdf:defsystem ,name
     ,@(progn (eval `(empty-keyword-fn ,name)) nil)
     ,@body))
(dspec:define-form-parser 
    (my-defsystem4
        (:parser 
         #.(dspec:get-form-parser 'defun))))
(macroexpand-1 '(my-defsystem4 :sys4 :components ((:file "file"))))
(my-defsystem4 :sys4 :components ((:file "file")))
:sys4
|#
;;;;;;;;;;;;;;;;;;;;;;;;;; 

;;;;;;;;;;;;;;;;;; Finished code ;;;;;;;;;;;;;;;;;;;;;
(setf (macro-function 'old-defsystem) (macro-function 'asdf:defsystem))

(defmacro new-defsystem (name &body body)
  (setf name (system-as-keyword name))
  (eval `(empty-keyword-fn ,name))
  `(old-defsystem ,name
     ,@body))

(setf (macro-function 'asdf:defsystem) (macro-function 'new-defsystem))
;(setf (macro-function 'asdf:defsystem) (macro-function 'old-defsystem))

(dspec:define-form-parser asdf:defsystem (name &rest body)
  `(defun ,(system-as-keyword name)))

;;;;;;;;; Redefining (for finding) this ASDF-system ;;;;;;;;
(unless *file-loaded*
  (setf *file-loaded* t)
  (load (asdf:system-source-file +this-sys+)))
;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(dspec:define-form-parser 
    (asdf:defsystem
        (:parser 
         #.(dspec:get-form-parser 'defun))))
|#
;;;;;;;;;; experements ;;;;;;;;;;;;;;;;;
#|
(defmacro mydefsystem (name &body body)
  (setf name (system-as-keyword name))
  (eval `(empty-keyword-fn ,name))
  `(old-defsystem ,name
     ,@body))
 
(dspec:define-form-parser mydefsystem (name &rest body)
  `(defun ,(system-as-keyword name)))

(mydefsys #:myexp7 :components ((:file "file")))

:myexp7
|#

;(editor:goto-buffer (editor:current-buffer) t)
;(defun 
;(capi:apply-in-pane-process ed 'capi:call-editor ed "Beginning Of Buffer")
;(capi:apply-in-pane-process ed 'capi:call-editor ed
;                            (list 'editor:forward-character-command
;                                  (slot-value pt 'editor::offset)  ))

;(when set-foreground-window 
;    (win32:set-foreground-window (window-handle-by-buffer buffer)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(macroexpand-1 '(asdf:defsystem :exp :components ((:file "file"))))
;(asdf:defsystem :exp :components ((:file "file")))
;(asdf:defsystem :exp2 :components ((:file "file")))
;:exp 
;:exp2
;:exp3
;(:exp)
;(editor:find-source-command ed :exp)
;(editor:find-source-command (capi:find-interface 'lispworks-tools:editor) :exp3)

