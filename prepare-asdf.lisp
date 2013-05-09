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

(dspec:define-form-parser 
    (asdf:defsystem
        (:parser 
         #.(dspec:get-form-parser 'defun))))
;(macroexpand-1 '(asdf:defsystem :exp :components ((:file "file"))))
;(asdf:defsystem :exp :components ((:file "file")))
;(asdf:defsystem :exp2 :components ((:file "file")))
;:exp 
;:exp2
;:exp3
;(:exp)
;(editor:find-source-command ed :exp)
;(editor:find-source-command (capi:find-interface 'lispworks-tools:editor) :exp3)