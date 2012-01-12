;;; Copyright (c) 2012, Rolando Pereira
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.


;;;; trivial-dump-core.lisp

(in-package #:trivial-dump-core)


;;; Main functions

(defun dump-image (filename)
  "Dump the current state of the lisp image into a file so that it can
be resumed in a later date."
  #+clisp (progn
            (format t "To run the image, use the following command in the terminal:
clisp -M ~A~%"
             filename)
            (ext:saveinitmem filename))
  #+sbcl (cond ((is-slime-running)
                 (print-dump-image-slime-help filename))
           (t
             (format t "To run the image, use the following command in the terminal:
sbcl --core ~A~%"
               filename)
             (sb-ext:save-lisp-and-die filename)))
  #+clozure (progn
              (format t "To run the image, use the following command in the terminal:
ccl -I ~A~%"
             filename)
              (ccl:save-application filename))
  #-(or clisp sbcl clozure) (error "The lisp implementation \"~A\" isn't supported.

Please consult its documentation to see how you can dump the lisp image."
                              (lisp-implementation-type))
  )

(defmacro save-executable (filename init-function &key (exit-repl t))
  "Dump the current lisp image into an executable.

FILENAME is a string representing the filename the image will have on the disk.

INIT-FUNCTION indicates the top-level function for the executable. It
needs to use the #' reader macro, ie. #'hello-world
and #'(lambda () (format t \"Hello, World!\") are fine, but
'hello-world is not.

The key EXIT-REPL if non-NIL indicates that the normal lisp REPL
should be started after running INIT-FUNCTION. Not every lisp implementation supports this keyword.

Pay attention to the fact that after running this
function (save-executable), some implementations (ie. sbcl and
clozure) quit the REPL."
  #+clisp `(%save-executable-clisp ,filename ,init-function ,exit-repl)
  #+sbcl `(%save-executable-sbcl ,filename ,init-function ,exit-repl)
  #+clozure `(%save-executable-clozure ,filename ,init-function ,exit-repl)
  #-(or clisp sbcl clozure) `(error "The lisp implementation \"~A\" isn't supported.

Please consult its documentation to see how you can create a executable."
                               (lisp-implementation-type))
  )


;;; Helper functions

(defun print-dump-image-slime-help (filename)
  "Print on the screen the command the user needs to run on the
*inferior-lisp* buffer to dump this image. This macro is only called
when trying to dump a sbcl lisp image inside slime."
  (format t "Cannot dump an sbcl image from inside Slime.

Please go to the *inferior-lisp* buffer in emacs and run the following code:

\(trivial-dump-core::sbcl-dump-image-slime \"~A\")"
     filename))

#+clisp 
(defun %save-executable-clisp (filename init-function exit-repl)
  (if exit-repl
    (ext:saveinitmem filename
      :init-function (lambda ()
                       (funcall init-function)
                       (ext:exit))
      :executable t)
    (ext:saveinitmem filename
      :init-function init-function
      :executable t)))

#+clozure
(defun %save-executable-clozure (filename init-function exit-repl)
  (if exit-repl
    (ccl:save-application filename
      :toplevel-function init-function
      :prepend-kernel t)
    (error "Option :EXIT-REPL with nil value not supported in Clozure CL."))

  ;; (ccl:save-application ,filename
  ;;   :toplevel-function (lambda ()
  ;;                        (funcall ,init-function)
  ;;                        (defparameter *application*
  ;;                          (make-instance 'lisp-development-system))
  ;;                        (toplevel-function *application*
  ;;                          (application-init-file *application*)))
  ;;                        ;; (setq toplevel-function 
  ;;                        ;;   #'(lambda ()
  ;;                        ;;       (toplevel-function *application*
  ;;                        ;;         (application-init-file *application*)))))
  ;;   :prepend-kernel t))

  )

(defmacro print-save-slime-and-die-help (filename init-function exit-repl)
  "Print on the screen the command the user needs to run on the
*inferior-lisp* buffer to save this image. This macro is only called
when trying to save a sbcl lisp image inside slime."
  `(format t "Cannot run save an sbcl image from inside Slime.

Please go to the *inferior-lisp* buffer in emacs and run the following code:

\(trivial-dump-core::sbcl-save-slime-and-die \"~A\" ~S~:[ :exit-repl nil~;~])"
     (quote ,filename) ,init-function (quote ,exit-repl)))

#+sbcl
(defmacro %save-executable-sbcl (filename init-function exit-repl)
  `(if (is-slime-running)
     (print-save-slime-and-die-help
       ,filename
       (quote ,init-function)
       ,exit-repl)
     (if ,exit-repl
       (sb-ext:save-lisp-and-die ,filename
         :toplevel ,init-function
         :executable t)
       (sb-ext:save-lisp-and-die ,filename
         :toplevel (lambda ()
                     (funcall ,init-function)
                     ;; Start the regular sbcl repl
                     (sb-impl::toplevel-init))
         :executable t))))

(defun is-slime-running ()
  "Return T if slime is running in the lisp image, otherwise return NIL."
  (when (member :swank *features*)
    ;; The use of read-from-string is necessary because if the code
    ;; tried to do 'swank::*connections* and the package 'swank didn't
    ;; exist (as is the case when running a lisp from the terminal),
    ;; the interpreter would throw an error when trying to load the
    ;; 'trivial-dump-core package.
    ;;
    ;; I wanted to avoid the use of #+swank conditionals because if
    ;; the interpreter started without :swank, and the user loaded
    ;; swank after the compilation of this file, then the user would
    ;; need to compile this function again so that this code path would exist.
    ;;
    ;; Therefore, the simpler way is to create the symbol at runtime
    ;; because if the code ever reaches this point, then the swank
    ;; package has to exist, due to it being in the *features*
    (let ((connections (string-to-symbol "swank::*connections*")))
      (not (zerop (length connections))))))

(defun string-to-symbol (string)
  "Convert a string to a symbol"
  (eval (read-from-string string)))

;; Based on:
;; http://badbyteblues.blogspot.com/2007/06/save-slime-and-die.html
#+sb-thread 
(defun sbcl-save-slime-and-die (filename init-function
                                 &key (exit-repl t))
  "Save a sbcl image, even when running from inside Slime.

This function should only be used in the *inferior-buffer* buffer,
inside emacs."
  (mapcar #'(lambda (x)
              (funcall
                (string-to-symbol "#'swank::close-connection")
                x nil nil))
    (string-to-symbol "swank::*connections*"))
  (dolist (thread (remove
                    (funcall (string-to-symbol "#'swank::current-thread"))
                    (funcall (string-to-symbol "#'swank::all-threads"))))
    (funcall (string-to-symbol "#'swank::kill-thread") thread))
  (sleep 1)
  (if exit-repl
    (sb-ext:save-lisp-and-die filename
      :toplevel init-function
      :executable t)
    (sb-ext:save-lisp-and-die filename
      :toplevel (lambda ()
                  (funcall init-function)
                  ;; Start the regular sbcl repl
                  (sb-impl::toplevel-init))
      :executable t)))

;; Based on:
;; http://badbyteblues.blogspot.com/2007/06/save-slime-and-die.html
#+sb-thread
(defun sbcl-dump-image-slime (filename)
  "Save a sbcl image, even when running from inside Slime.

This function should only be used in the *inferior-buffer* buffer,
inside emacs."
  (mapcar #'(lambda (x)
              (funcall
                (string-to-symbol "#'swank::close-connection")
                x nil nil))
    (string-to-symbol "swank::*connections*"))
  (dolist (thread (remove
                    (funcall (string-to-symbol "#'swank::current-thread"))
                    (funcall (string-to-symbol "#'swank::all-threads"))))
    (funcall (string-to-symbol "#'swank::kill-thread") thread))
  (sleep 1)
  (format t "To run the image, use the following command in the terminal:
sbcl --core ~A~%"
    filename)
  (sb-ext:save-lisp-and-die filename))
