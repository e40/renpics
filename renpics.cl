;; $Id$

(in-package :user)

(setq *read-init-files* nil)
(setq excl::*internal-read-init-files* nil)
(setq *print-startup-message* nil)
(setq excl::.dump-lisp-suppress-allegro-cl-banner. t)
(setq excl::*force-quiet-exit* t) ; 6.0
(setq sys::.ignore-command-line-arguments. t)

(eval-when (compile)
  (compile-file-if-needed "c:/src/exif-utils/exifdump.cl"))

(eval-when (compile eval load)
  (require :exifdump "c:/src/exif-utils/exifdump.fasl")
  (require :aclwin)
  (require :fileutil))

(defvar *quiet* nil)
(defvar *no-execute* nil)
(defvar *debug* nil)
(defvar *usage* "Usage: [-n] [-m] [-q] [-o output-directory] directory~%")

(defun renpics-init-function ()
  (handler-case
      (sys:with-command-line-arguments
	  ("I:o:mnq"
	   image-file output-directory move-images *no-execute* *quiet*)
	  (rest)
	(declare (ignore image-file))
	(when (/= 1 (length rest)) (error-die *usage*))
	(renpics (first rest) output-directory :move-images move-images)
	(exit 0 :quiet t))
    (error (c) (error-die "An error occurred: ~a." c))))

(defun renpics (source-directory output-directory
		&key move-images)
  (when (not (probe-file source-directory))
    (error-die "Directory ~a does not exist." source-directory))
  (map-over-directory
   #'(lambda (p)
       (when (equalp "jpg" (pathname-type p))
	 (handler-case
	     (let ((new-p (exif-based-name p output-directory)))
	       (if* (equalp (file-namestring p)
			    (file-namestring new-p))
		  thenret ;; do nothing
		elseif *no-execute*
		  then (when (not *quiet*)
			 (format t "rename ~a to ~a~%" p new-p))
		  else (when (not *quiet*)
			 (format t "renaming ~a to ~a~%" p new-p))
		       (when (and output-directory (probe-file new-p))
			 (error "~a already exists!" new-p))
		       (if* move-images
			  then (rename-file p new-p)
			  else (sys:copy-file p new-p))))
	   (error (c)
	     (let ((*print-pretty* nil))
	       (format t "Skipping ~a:~%  ~a~%" p c))))))
   source-directory))

(defun exif-based-name (file output-directory)
  (let* ((exif-info (parse-exif-data file))
	 (raw-camera (exif-info-model exif-info))
	 (camera (if* (string= "Canon EOS D30" raw-camera)
		    then "D30"
		  elseif (or (string= "E950" raw-camera)
			     ;; hack for me:
			     (match-regexp
			      (load-time-value (compile-regexp "^nikon"))
			      (file-namestring file)
			      :case-fold t :return nil))
		    then "N950"
		  elseif (member raw-camera '("E990" "E800" "E700")
				 :test 'string=)
		    then raw-camera
		  elseif (string= "DSC-D700" raw-camera)
		    then "D700"
		  elseif (string= "Canon PowerShot S20" raw-camera)
		    then "S20"
		  elseif (string= "Canon PowerShot G1" raw-camera)
		    then "G1"
		  elseif (match-regexp
			   (load-time-value (compile-regexp "NIKON D1"))
			   raw-camera
			   :case-fold t :return nil)
		    then "D1"
		  elseif (or
			  (match-regexp
			   (load-time-value (compile-regexp "^DC210"))
			   raw-camera
			   :case-fold t :return nil)
			  ;; hack for me:
			  (match-regexp
			   (load-time-value (compile-regexp "^dcp"))
			   (file-namestring file)
			   :case-fold t :return nil))
		    then "DC210"
		    else (error "Unknown camera: ~a." raw-camera)))
	 name
	 (sequence 0))
    (destructuring-bind (year month day hour minute second)
	(mapcar #'read-from-string
		(split-regexp
		 (load-time-value (compile-regexp "[: ]"))
		 (or (exif-info-date exif-info)
		     (error "No :date in exif info."))))
      (when (zerop year)
	;; pictures taken before time set on camera...
	(multiple-value-setq (second minute hour day month year)
	  (decode-universal-time (file-write-date file))))
      (loop
	(setq name
	  (format nil "~d~2,'0d~2,'0d-~2,'0d~2,'0d-~2,'0d~2,'0d-~a.~a"
		  year month day hour minute second
		  sequence
		  camera
		  (pathname-type file)))
	(let ((new (merge-pathnames name
				    (if* output-directory
				       thenret
				       else file))))
	  (when (not (probe-file new))
	    (return-from exif-based-name new)))
	(incf sequence)))))

#+ignore
(defun read-exif-info (file)
  (multiple-value-bind (s error-s pid)
      (progn
	(chdir (excl::path-pathname file))
	(run-shell-command
	 (format nil "bash -c \"exifhead.exe '~a'\"" (file-namestring file))
	 :output :stream
	 :error-output :stream
	 :wait nil
	 :show-window :hide))
    (unwind-protect
	(let ((exif-info (read s nil s)))
	  (when (null exif-info)
	    (error "could not read exif info from ~a." file))
	  exif-info)
      (sys:reap-os-subprocess :pid pid :wait t)
      (ignore-errors (close s))
      (ignore-errors (close error-s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; utils which should be in allegro?????

(defun error-die (format &rest args)
  (if* *debug*
     then (apply #'break format args)
     else (apply #'format t format args)
	  (fresh-line t)
	  (exit 0 :no-unwind t :quiet t)))

(defun my-read-line (stream &optional terminate-on-space)
  (let ((term-chars
	 `(#\return #\newline ,@(when terminate-on-space '(#\space))))
	(line '())
	(eof nil)
	c)
    (loop
      (setq c (read-char stream nil stream))
      (when (or (and (char= c stream) (setq eof t))
		(and (member c term-chars :test #'char=)
		     ;; read-char until no more term-chars or EOF
		     (loop
		       (setq c (peek-char nil stream nil stream))
		       (when (or (and (char= c stream) (setq eof t))
				 (not (member c term-chars :test #'char=)))
			 (return t))
		       ;; toss it
		       (read-char stream))))
	(return
	  (if* (and eof (null line))
	     then stream
	     else (concatenate 'simple-string (nreverse line)))))
      (push c line))))

(defun read-lines (stream)
  (let ((lines '())
	line)
    (loop
      (setq line (my-read-line stream))
      (when (eq stream line) (return (nreverse lines)))
      (push line lines))))

(provide :renpics)
