(use-package :foil)
(def-foil-class "System.Object")
(def-foil-class "System.Reflection.Assembly")
(def-foil-class "System.Reflection.Module")
(def-foil-class "System.Type")
(def-foil-class "System.Collections.IEnumerator")
(def-foil-class "System.Collections.IEnumerable")

(use-package "System")
(use-package "System.Reflection")
(use-package "System.Collections")

(defmacro doenum ((e enum) &body body)
  (let ((genum (gensym)))
    `(let ((,genum ,enum))
       (do ()
           ((not (ienumerator.movenext ,genum)))
         (let ((,e (ienumerator.current ,genum)))
           ,@body)))))

(defun get-assembly-classnames (assembly-file-name &rest packages)
  "returns a list of strings, assemblies should be of the form \"system/io\"
  for recursive lookup and \"system/io/\" for non-recursive"
  (let* ((asm (assembly.loadfrom assembly-file-name))
         (types (ienumerable.GetEnumerator (assembly.GetTypes asm)))
         (dot-packages (mapcar (lambda (p)
                                 (substitute #\. #\/ p)) packages))
         (names ()))
    (doenum (e types)
      (let ((ename (|System|::Type.tostring e)))
          (format t "~A~%" ename)
        (flet ((matches (package)
                 (and (eql 0 (search package ename))
                      (or (not (eql #\. (schar package (1- (length package))))) ;recursive
                          (not (find #\. ename :start (length package))))))) ;non-subdirectory
          (when  (and ;don't grab implementation details classes
                  (not (or (find #\$ ename)
                           (find #\+ ename)
                           (search "__" ename)
                           (search "PrivateImplementationDetails" ename)))
                  (some #'matches dot-packages))
            (push ename names)))))
    names))


(defun gen-wrappers ()
  (dump-wrapper-defs-to-file "/foil/cli-system-collections.lisp" (get-assembly-classnames "/WINDOWS/Microsoft.NET/Framework/v1.1.4322/mscorlib.dll" "System.Collections"))
;  (dump-wrapper-defs-to-file "/foil/cli-system-windows-forms.lisp" (get-assembly-classnames "/WINDOWS/Microsoft.NET/Framework/v1.1.4322/System.Windows.Forms.dll" "System/Windows/Forms/"))
  (compile-file "/foil/cli-system-collections.lisp")
)
 
;(gen-wrappers) 

(defun test-hash-indexer()
  (let ((ht (hashtable.new)))
    (setf (foil::iref ht "Fred") 50
          (foil::iref ht "Betty") 100
          (foil::iref ht 100) nil
          (foil::iref ht 23.4) t
          (foil::iref ht "More") " and more")
    (doenum (e (ienumerable.getenumerator (hashtable.keys ht)))
               (format t "key=~A value=~A~%" e (foil::iref ht e)))))

