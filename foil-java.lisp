(load "/dev/foil/foil")
(use-package :foil)
(def-foil-class "java.util.jar.JarFile")
(def-foil-class "java.util.Enumeration")
(def-foil-class "java.util.zip.ZipEntry")

(use-package "java.util")
(use-package "java.util.jar")
(use-package "java.util.zip")

(defmacro doenum ((e enum) &body body)
  (let ((genum (gensym)))
    `(let ((,genum ,enum))
       (do ()
           ((not (enumeration.hasmoreelements ,genum)))
         (let ((,e (enumeration.nextelement ,genum)))
           ,@body)))))

(defun get-jar-classnames (jar-file-name &rest packages)
  "returns a list of strings, packages should be of the form \"java/lang\"
  for recursive lookup and \"java/util/\" for non-recursive"
  (let* ((jar (jarfile.new jar-file-name))
         (entries (jarfile.entries jar))
         (names ()))
    (doenum (e entries)
      (unless (zipentry.isdirectory e)
        (let ((ename (zipentry.getname e)))
          (flet ((matches (package)
                   (and (eql 0 (search package ename))
                        (or (not (eql #\/ (schar package (1- (length package))))) ;recursive
                            (not (find #\/ ename :start (length package))))))) ;non-subdirectory
            (when (and (eql (search ".class" ename)
                            (- (length ename) 6)) ;classname
                       ;don't grab anonymous inner classes
                       (not (and (find #\$ ename)
                                 (digit-char-p (schar ename (1+ (position #\$ ename))))))
                       (some #'matches packages))
              (push (nsubstitute #\. #\/ (subseq ename 0 (- (length ename) 6)))
                    names))))))
    names))


(defun gen-wrappers ()
;/System/Library/Frameworks/JavaVM.framework/Versions/1.4.2/Classes/classes.jar
#+nil(dump-wrapper-defs-to-file "~/foil/swt.lisp" (get-jar-classnames "/Users/rich/downloads/eclipse/plugins/org.eclipse.swt.carbon_3.0.0/ws/carbon/swt.jar" "org/eclipse/swt/" "org/eclipse/swt/program/" "org/eclipse/swt/printing/" "org/eclipse/swt/layout/" "org/eclipse/swt/dnd/" "org/eclipse/swt/graphics/" "org/eclipse/swt/custom/" "org/eclipse/swt/events/" "org/eclipse/swt/printing/" "org/eclipse/swt/widgets/" "org/eclipse/swt/browser/" "org/eclipse/swt/awt/"))

  (dump-wrapper-defs-to-file "/foil/java-lang.lisp" (get-jar-classnames "/j2sdk1.4.2/jre/lib/rt.jar" "java/lang/"))
  (dump-wrapper-defs-to-file "/foil/java-util.lisp" (get-jar-classnames "/j2sdk1.4.2/jre/lib/rt.jar" "java/util/"))
  (dump-wrapper-defs-to-file "/foil/java-io.lisp" (get-jar-classnames "/j2sdk1.4.2/jre/lib/rt.jar" "java/io/"))
  (dump-wrapper-defs-to-file "/foil/java-sql.lisp" (get-jar-classnames "/j2sdk1.4.2/jre/lib/rt.jar" "java/sql/"))
  (dump-wrapper-defs-to-file "/foil/javax-sql.lisp" (get-jar-classnames "/j2sdk1.4.2/jre/lib/rt.jar" "javax/sql/"))
  (dump-wrapper-defs-to-file "/foil/swt.lisp" (get-jar-classnames "/dev/eclipse/plugins/org.eclipse.swt.win32_3.0.0/ws/win32/swt.jar" "org/eclipse/swt/" "org/eclipse/swt/program/" "org/eclipse/swt/printing/" "org/eclipse/swt/layout/" "org/eclipse/swt/dnd/" "org/eclipse/swt/graphics/" "org/eclipse/swt/custom/" "org/eclipse/swt/events/" "org/eclipse/swt/printing/" "org/eclipse/swt/widgets/" "org/eclipse/swt/browser/" "org/eclipse/swt/awt/"))

  (compile-file "/foil/java-lang")
  (compile-file "/foil/java-util")
  (compile-file "/foil/java-io")
  (compile-file "/foil/java-sql")
  (compile-file "/foil/javax-sql")
  (compile-file "/foil/swt"))
