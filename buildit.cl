
(in-package :user)

(setq excl::*break-on-warnings* t)

(delete-directory-and-files "c:/src/renpics/dist/" :if-does-not-exist :ignore)

(declaim (optimize (speed 3)))
(compile-file "renpics.cl")

(generate-application
 "renpics"
 "renpics/"
 '(:seq2 "renpics.fasl")
 :application-files '("renpics.txt")
 :restart-init-function 'main
 :include-ide nil
 :include-compiler nil
 :us-government nil
 :presto nil
 :discard-local-name-info t
 :discard-source-file-info t
 :discard-xref-info t
 :load-xref-info nil
 :load-source-file-info nil
 :record-xref-info nil
 :record-source-file-info nil
 :include-devel-env nil
 :include-tpl nil
 :newspace 6144
 :oldspace 256000
 :show-window :normal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 :autoload-warning nil
 :purify nil
 :runtime :standard
 :suppress-allegro-cl-banner t
 :print-startup-message nil		; don't print ACL startup messages
 :read-init-files nil			; don't read ACL init files
 :ignore-command-line-arguments t	; ignore ACL (not app) cmd line options
 )

#+mswindows
(progn
  (delete-file "renpics/renpics.exe")
  (sys:copy-file "c:/Program files/acl62/buildi.exe" "renpics/renpics.exe"))

;; I believe :ignore-command-line-arguments makes the following unncessary
#+ignore
(with-open-file (s "renpics/renpics.rc" :direction :output)
  (format s ".command-line: --~%"))

