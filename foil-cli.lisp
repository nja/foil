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
                           ;(search "__" ename)
                           (search "PrivateImplementationDetails" ename)))
                  (some #'matches dot-packages))
            (push ename names)))))
    names))


(defun gen-wrappers ()
  (dump-wrapper-defs-to-file "/foil/cli-system.lisp" (get-assembly-classnames "/WINDOWS/Microsoft.NET/Framework/v1.1.4322/mscorlib.dll" "System/"))
;  (dump-wrapper-defs-to-file "/foil/cli-system-windows-forms.lisp" (get-assembly-classnames "/WINDOWS/Microsoft.NET/Framework/v1.1.4322/System.Windows.Forms.dll" "System/Windows/Forms/"))
 
  ;(dump-wrapper-defs-to-file "/foil/java-util.lisp" (get-jar-classnames "/j2sdk1.4.2/jre/lib/rt.jar" "java/util/"))
  ;(dump-wrapper-defs-to-file "/foil/java-io.lisp" (get-jar-classnames "/j2sdk1.4.2/jre/lib/rt.jar" "java/io/"))
  ;(dump-wrapper-defs-to-file "/foil/java-sql.lisp" (get-jar-classnames "/j2sdk1.4.2/jre/lib/rt.jar" "java/sql/"))
  ;(dump-wrapper-defs-to-file "/foil/javax-sql.lisp" (get-jar-classnames "/j2sdk1.4.2/jre/lib/rt.jar" "javax/sql/"))
  ;(dump-wrapper-defs-to-file "/foil/swt.lisp" (get-jar-classnames "/swt/swt.jar" "org/eclipse/swt/" "org/eclipse/swt/program/" "org/eclipse/swt/printing/" "org/eclipse/swt/layout/" "org/eclipse/s;wt/dnd/" "org/eclipse/swt/graphics/" "org/eclipse/swt/custom/" "org/eclipse/swt/events/" "org/eclipse/swt/printing/" "org/eclipse/swt/widgets/" "org/eclipse/swt/browser/" "org/eclipse/swt/awt/"))

  (compile-file "/foil/cli-system.lisp")
  (compile-file "/foil/cli-system-windows-forms.lisp")
 ; (compile-file "/foil/java-io")
 ; (compile-file "/foil/java-sql")
 ; (compile-file "/foil/javax-sql")
 ; (compile-file "/foil/swt")
)
 
;(gen-wrappers)