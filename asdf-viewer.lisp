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
    (print (list 'outer sys))
    (dolist (dep (append (system-depends-on sys :as-systems t :defsystem-depends-on t)
                         (system-depends-on sys :as-systems t :defsystem-depends-on nil)))
      (print (list 'inner (list sys dep)))
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




;(sys-deps (find-system "sw-init"))
;(system-depends-on (find-system "sw-init") :as-systems t :defsystem-depends-on t)



#|(contain 
 (make-instance 'tree-view
                :roots (mapcar #'package-name (list-all-packages))
                :children-function 'sys-deps (mapcar #'package-name (package-use-list (find-package "RESTAS")))
                (mapcar #'package-name (package-used-by-list (find-package "ITERATE")))
                ))
|#


;(defun get-definition-location (symbol)
;  (second (first (dspec:find-name-locations '(package) symbol))))

;(get-definition-location :CFFI)
;(dspec:find-name-locations '(asdf::defsystem) :cffi)
;;;;;;;;;;;;

;(ql:quickload :alexandria)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(goto-or-create-xy (system-source-file :cffi) 0 0)

(defmacro let1 (variable + &body progn) 
  "Shortcut for (let ((a b)) . c) or (destructuring-bind a b . c)"
  (if (atom variable)
      `(let ((,variable ,+)) ,@progn)
    `(destructuring-bind ,variable ,+ ,@progn)))

(defun window-handle-by-buffer (buffer)
  "Возвращает handle первого окна данного буфера"
  (assert (typep buffer 'editor::buffer))
  (let1 window (car (editor:buffer-windows buffer))
    (let1 pane (slot-value window 'editor::text-pane)
      (let1 representation (slot-value pane 'capi-internals:representation)
        (slot-value representation 'win32:hwnd)))))

(defun goto-buffer-2 (buffer in-same-window &key (set-foreground-window t))
  "Не только отображает буфер, но и показывет окно"
  (editor:goto-buffer buffer in-same-window)
  (when set-foreground-window 
    (win32:set-foreground-window (window-handle-by-buffer buffer)))
  )

(defun ignored (&rest ignore) 
        (declare (ignore ignore)) nil)

(defmacro ignore-errors-2 (&body body)
  (alexandria:with-gensyms (results)
  `(let1 ,results
       (multiple-value-list (ignore-errors ,@body))
     (when (and ; got a error
            (= (length ,results) 2)
            (null (first ,results))
            (typep (second ,results) 'condition)
            )
       (format *error-output* "ignore-errors-2 пропускает ошибку (чёрные иконки?) ~A" (second ,results))
       (format *standard-output* "ignore-errors-2 пропускает ошибку (чёрные иконки?) ~A" (second ,results))
       (format *trace-output* "ignore-errors-2 пропускает ошибку (чёрные иконки?) ~A" (second ,results))
       )
     (values-list ,results))))

#-lispworks6.1
(defun get-some-editor () 
  (loop for x in capi-win32-lib::*top-level-windows* 
              for elt = (slot-value x 'win32::element) 
              when (starts-with-subseq "Editor " (slot-value elt 'capi::title))
              do (return #+lispworks4 (slot-value elt 'capi:editor-pane) #+lispworks6 elt)))

#+lispworks6.1
(defun get-some-editor ()
  "SHould return the listener itself, but returns interactive-pane"
  (let* ((interfaces (slot-value (car capi-win32-lib::*screens*) 'capi::interfaces)))
    (assert interfaces () "the-listener: unable to find interfaces")
    (let ((editor
           (find 'lispworks-tools:editor interfaces :key 'type-of)))
      editor)))

(defun goto-or-create-xy (pathname row col &key kill-buffer (set-foreground-window t))
  (capi:find-interface 'lispworks-tools:editor)
  (goto-xy pathname row col :kill-buffer kill-buffer :set-foreground-window set-foreground-window))

(defun goto-xy (pathname row col &key kill-buffer (set-foreground-window t))
  "Не сработает при отсутствии редактора. При kill-buffer опасно, т.к. закрывает файл без изменений"
  (let1 ed (get-some-editor)
    (unless ed (error "Не могу показать исходника - нет окна редактора!"))
    (
     #+lispworks6 capi:execute-with-interface 
                  #+lispworks4 capi:apply-in-pane-process
                  ed
                  (lambda ()
                     (ignore-errors-2 
                      (editor::switch-to-or-from-typeout-command 0))
                     (let ((buf (editor::find-file-in-buffer-list pathname #'ignored)))
                       (when (and buf kill-buffer)
                         (editor::kill-buffer-no-confirm buf))
                       (unless buf
                         (setf buf (editor:find-file-buffer pathname #'ignored)))
                       (goto-buffer-2 buf t :set-foreground-window set-foreground-window))
                    (capi:call-editor ed "Beginning Of Buffer" )
                    (when (> row 1) (ignore-errors-2 (editor:next-line-command (- ROW 1))))
                    (when (> col 1) 
                      (ignore-errors-2 
                        (editor:forward-character-command (- COL 1))))
                    (ignore-errors-2 (goto-buffer-2 (editor:find-file-buffer pathname) t :set-foreground-window set-foreground-window))
                    ))
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-asd-file (sys)
  (goto-or-create-xy (system-source-file sys) 0 0))

(defun show-system-callback (data interface)
  (declare (ignore interface))
  (open-asd-file data))

(contain 
 (make-instance 'tree-view
                :title "ASDF-systems and their dependencies"
                :roots (get-all-systems)
                :children-function 'sys-deps
                :action-callback #'show-system-callback
                ))

(fill-reverse-deps-hash)

(contain 
 (make-instance 'tree-view
                :title "ASDF-systems depends on"
                :roots (get-all-systems :as-systems t)
                :print-function #'coerce-name
                :children-function 'sys-reverse-deps 
                :action-callback #'show-system-callback
                ))
