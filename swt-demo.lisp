(load "/dev/foil/foil")
(use-package :foil)
(load "/foil/java-lang")
(load "/foil/java-util")
(load "/foil/swt")
(require "comm")

(defpackage :swt-demo
 (:use :cl :foil "org.eclipse.swt" "org.eclipse.swt.widgets" "org.eclipse.swt.events")
 (:export
  :*display*
  :init-display
  :show-shell))
(in-package :swt-demo)

;presumes swt-aware java server running on 2 ports, first will be ui

(defvar *ui-stream*)
(defvar *non-ui-stream*)
(defvar *display*)

(setf *ui-stream* (comm:open-tcp-stream "localhost" 13578))
(setf *non-ui-stream* (comm:open-tcp-stream "localhost" 13579))
(setf *fvm* (make-instance 'foreign-vm :stream *non-ui-stream*))

(def-foil-class "com.richhickey.foil.SWTHelper")


(defun init-display ()
  (let ((*thread-fvm* *fvm*)
        (*thread-fvm-stream* *ui-stream*))
    (setf *display* (make-new display.))))

(defun show-shell ()
  (let* ((*thread-fvm* *fvm*)
         (*thread-fvm-stream* *ui-stream*)
         (shell (make-new shell. *display* :text "Using SWT from Lisp"))
         (button (make-new button. shell *SWT.CENTER* :text "Call Lisp"))
         (listener (make-new-proxy +MARSHALL-ID+ 1 mouselistener. selectionlistener.)))
    (shell.setsize shell 300 200)
    (shell.setlocation shell 100 100)
    (button.setsize button 200 100)
    (button.setlocation button 40 40)
    (button.addmouselistener button listener)
    (button.addselectionlistener button listener)
    (let ((mp:*process-initial-bindings*
           (append '((*standard-output* . *standard-output*)
                     (*fvm* . *fvm*)
                     (*thread-fvm-stream* . *thread-fvm-stream*)
                     (*thread-fvm* . *thread-fvm*))
                     mp:*process-initial-bindings*)))
      #+nil(|com.richhickey.foil|::swthelper.rundispatchloop *display* shell)
      (mp:process-run-function
       "swt-proc" '()
       (lambda ()
         (|com.richhickey.foil|::swthelper.rundispatchloop *display* shell))))))

(defmethod handle-proxy-call ((method (eql 'selectionlistener.widgetselected)) proxy &rest args)
  (let* ((ev (first args))
        (button (cdr (assoc :source (fref-val ev)))))
    (setf (button.text button) "Hello from Lisp")))
