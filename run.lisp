(push (make-pathname :defaults *load-pathname* :name nil :type nil) asdf:*central-registry*)
(asdf:load-system :lisp-world-viewer)
(asdf-viewer:define-lisp-world-viewer-and-show)
