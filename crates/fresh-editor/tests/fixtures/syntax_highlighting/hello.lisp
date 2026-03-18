;;; Common Lisp syntax highlighting test
(defun greet (name)
  "Return a greeting string"
  (format nil "Hello, ~A!" name))

(defvar *config*
  '(:version "1.0" :enabled t :count 42))

(defun main ()
  (let ((message (greet "World")))
    (format t "~A~%" message)
    (loop for i from 1 to 5
          do (format t "Item: ~D~%" i))))
