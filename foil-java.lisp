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