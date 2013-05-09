
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
#|
(defmacro let1 (variable + &body progn) 
  "Shortcut for (let ((a b)) . c) or (destructuring-bind a b . c)"
  (if (atom variable)
      `(let ((,variable ,+)) ,@progn)
    `(destructuring-bind ,variable ,+ ,@progn)))

(defun window-handle-by-buffer (buffer)
  "Âîçâðàùàåò handle ïåðâîãî îêíà äàííîãî áóôåðà"
  (assert (typep buffer 'editor::buffer))
  (let1 window (car (editor:buffer-windows buffer))
    (let1 pane (slot-value window 'editor::text-pane)
      (let1 representation (slot-value pane 'capi-internals:representation)
        (slot-value representation 'win32:hwnd)))))

(defun goto-buffer-2 (buffer in-same-window &key (set-foreground-window t))
  "Íå òîëüêî îòîáðàæàåò áóôåð, íî è ïîêàçûâåò îêíî"
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
       (format *error-output* "ignore-errors-2 ïðîïóñêàåò îøèáêó (÷¸ðíûå èêîíêè?) ~A" (second ,results))
       (format *standard-output* "ignore-errors-2 ïðîïóñêàåò îøèáêó (÷¸ðíûå èêîíêè?) ~A" (second ,results))
       (format *trace-output* "ignore-errors-2 ïðîïóñêàåò îøèáêó (÷¸ðíûå èêîíêè?) ~A" (second ,results))
       )
     (values-list ,results))))
 
#-lispworks6.1
(defun get-some-editor () 
  (loop for x in capi-win32-lib::*top-level-windows* 
              for elt = (slot-value x 'win32::element) 
              when (starts-with-subseq "Editor " (slot-value elt 'capi::title))
              do (return #+lispworks4 (slot-value elt 'capi:editor-pane) #+lispworks6 elt)))


#+lispworks6.1
#|(defun get-some-editor ()
  "SHould return the listener itself, but returns interactive-pane"
  (let* ((interfaces (slot-value (car capi-win32-lib::*screens*) 'capi::interfaces)))
    (assert interfaces () "the-listener: unable to find interfaces")
    (let ((editor
           (find 'lispworks-tools:editor interfaces :key 'type-of)))
      editor)))|#
(defun get-some-editor ()
  (capi:find-interface 'lispworks-tools:editor))

(defun goto-or-create-xy (pathname row col &key kill-buffer (set-foreground-window t))
  (capi:find-interface 'lispworks-tools:editor)
  (goto-xy pathname row col :kill-buffer kill-buffer :set-foreground-window set-foreground-window))

(defun goto-xy (pathname row col &key kill-buffer (set-foreground-window t))
  "Íå ñðàáîòàåò ïðè îòñóòñòâèè ðåäàêòîðà. Ïðè kill-buffer îïàñíî, ò.ê. çàêðûâàåò ôàéë áåç èçìåíåíèé"
  (let1 ed (get-some-editor)
    (unless ed (error "Íå ìîãó ïîêàçàòü èñõîäíèêà - íåò îêíà ðåäàêòîðà!"))
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
|#


