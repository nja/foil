(use-package :foil)
(use-package "System")
(use-package "System.Windows.Forms")

;presumes swt-aware java server running on 2 ports, first will be ui

#|
(defvar *ui-stream*)
(defvar *non-ui-stream*)
(defvar *display*)

(setf *ui-stream* (comm:open-tcp-stream "localhost" 13479))
(setf *non-ui-stream* (comm:open-tcp-stream "localhost" 13478))
(setf *fvm* (make-instance 'foreign-vm :stream *non-ui-stream*))
|#

;(defun init-tree-view ()
;  (let ((tv (TreeView.new 

(defvar *my-form* nil)

(defun show-app()
  (let ((form (Form.new)))
    (setf *my-form* form)
    (Form.SuspendLayout form)
    (setf (Form.AutoScaleBaseSize form) (|System.Drawing|::Size.new 5  13)
          (Form.ClientSize form) (|System.Drawing|::Size.new 856 654))
    (Form.ResumeLayout form nil)
    (let ((mp:*process-initial-bindings*
     (append '((*standard-output* . *standard-output*)
               (*fvm* . *fvm*)
               (*thread-fvm-stream* . *thread-fvm-stream*)
               (*thread-fvm* . *thread-fvm*))
             mp:*process-initial-bindings*)))
      (mp:process-run-function
       "winform-proc" '()
       (lambda ()
         (|System.Windows.Forms|::Application.Run form))))))




    