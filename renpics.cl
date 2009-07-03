;; Rename picture files based on date they were taken.
;;
;; This software is Copyright (c) Kevin Layer, 2000-2005.
;; Kevin Layer grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Revision$

(in-package :user)

(eval-when (compile)
  (compile-file-if-needed "exif-utils/exifinfo.cl"))

(eval-when (compile eval load)
  (require :exifinfo "exif-utils/exifinfo.fasl")
  (use-package :util.exif)

  (require :aclwin)
  (require :fileutil))

(defvar *quiet* nil)
(defvar *no-execute* nil)
(defvar *debug* nil)
(defvar *usage*
    "Usage: [-c camera] [-n] [-m] [-q] [-o output-directory] directory
-c camera :: force camera name to `camera'
-f :: use file write date if there is no exif information in file
-n :: just print what would be done, but do not do it
-m :: move, instead of copy, images to destination directory
-q :: operate quietly
-o output-directory :: send files to output-directory
directory :: the source location for pictures to rename

What the program does is look inside the jpg's at the EXIF info and pulls
out the date and camera name.  The camera name is assigned an
abbreviation.  The final name of the file has this format:

  YYYYMMDD-hhmm-ssnn-xxxx.jpg

where YYYY is the year, MM is the month, DD is the day, hh is the hour, mm
is the minute, ss is the second, nn is a sequence number used only when
more than one picture was taken in a given second, and xxxx is the camera
name.

NOTE: if you run renpics more than once on the same directory and do
not give a -o argument, it will keep renaming the files, updating only the
nn (sequence number) value discussed above.
")

(defun main ()
  (handler-case
      (sys:with-command-line-arguments
	  ("c:fo:mnq"
	   camera file-date output-directory move-images
	   *no-execute* *quiet*)
	  (rest)
	(when (/= 1 (length rest))
	  (format t "~&You must provide an input directory.~%")
	  (error-die *usage*))
	(renpics (first rest) output-directory :move-images move-images
		 :camera camera :file-date file-date)
	(exit 0 :quiet t))
    (error (c) (error-die "An error occurred: ~a." c))))

(defun renpics (source-directory output-directory
		&key camera file-date move-images
		&aux (nfiles 0) (nmovies 0) (nfile 0) (nmovie 0))
  (when (not (probe-file source-directory))
    (error-die "Directory ~a does not exist." source-directory))

  (map-over-directory (lambda (p)
			(when (equalp "jpg" (pathname-type p)) (incf nfiles))
			(when (or (equalp "avi" (pathname-type p))
				  (equalp "mov" (pathname-type p)))
			  (incf nmovies)))
		      source-directory)
  (when (not *quiet*)
    (when (> nfiles 0)
      (format t "~d images on card.~%" nfiles))
    (when (> nmovies 0)
      (format t "~d movies on card.~%" nmovies))
    (format t "~%"))

  ;; Do .mov files first, because they have a companion .jpg file, which we
  ;; need for the exif info, and then we delete it.
  (map-over-directory
   (lambda (p &aux type)
     (when (equalp "mov" (setq type (pathname-type p)))
       (handler-case
	   (let* ((jpg-type (make-pathname :type "jpg"))
		  (jpg (merge-pathnames (merge-pathnames jpg-type p)
					source-directory))
		  (new-p
		   (progn
		     (when (not (probe-file jpg))
		       (error "jpg file ~a does not exist!" jpg))
		     (merge-pathnames
		      (make-pathname :type type)
		      (exif-based-name jpg output-directory camera
				       file-date))))
		  (op (if* move-images then "move" else "copy")))
	     (when (not *quiet*)
	       (incf nmovie)
	       (format t "~d: ~a ~a to ~a~%"
		       nmovie op (file-namestring p) new-p)
	       (when move-images
		 (format t "   remove ~a~%" (file-namestring jpg))))
	     (when (not *no-execute*)
	       (if* move-images
		  then (rename-file p new-p)
		       (delete-file jpg)
		  else (sys:copy-file p new-p))))
	 (error (c)
	   (let ((*print-pretty* nil))
	     (format t "Skipping ~a:~%  ~a~%" p c)))))

     (when (equalp "avi" (setq type (pathname-type p)))
       (handler-case
	   (let* ((thm-type (make-pathname :type "thm"))
		  (thm (merge-pathnames thm-type p))
		  (new-p
		   (progn
		     (when (not (probe-file thm))
		       (error "thm file ~a does not exist!" thm))
		     (merge-pathnames
		      (make-pathname :type type)
		      (exif-based-name thm output-directory camera
				       file-date))))
		  (new-thm (merge-pathnames thm-type new-p))
		  (op (if* move-images then "move" else "copy")))
	     (when (not *quiet*)
	       (incf nmovie)
	       (format t "~d: ~a ~a to ~a~%"
		       nmovie op (file-namestring p) new-p)
	       (format t "   ~a ~a to ~a~%"
		       op (file-namestring thm) new-thm))
	     (when (not *no-execute*)
	       (if* move-images
		  then (rename-file p new-p)
		       (rename-file thm new-thm)
		  else (sys:copy-file p new-p)
		       (sys:copy-file thm new-thm))))
	 (error (c)
	   (let ((*print-pretty* nil))
	     (format t "Skipping ~a:~%  ~a~%" p c))))))
   source-directory)

  (map-over-directory
   (lambda (p)
     (when (equalp "jpg" (pathname-type p))
       (handler-case
	   (let* ((new-p (exif-based-name p output-directory camera
					  file-date))
		  (wav-type (make-pathname :type "wav"))
		  (wav (merge-pathnames wav-type p))
		  (new-wav (merge-pathnames wav-type new-p))
		  (wav-exists (probe-file wav)))
	     (when (not *quiet*)
	       (incf nfile)
	       (format t "~d: ~a ~a to ~a~%"
		       nfile (if* move-images then "move" else "copy")
		       (file-namestring p)
		       new-p)
	       (when wav-exists
		 (format t "   ~a to ~a~%" (file-namestring wav)
			 new-wav)))
	     (when (not *no-execute*)
	       (if* move-images
		  then (rename-file p new-p)
		  else (sys:copy-file p new-p))
	       (when wav-exists
		 (if* move-images
		    then (rename-file wav new-wav)
		    else (sys:copy-file wav new-wav)))))
	 (error (c)
	   (let ((*print-pretty* nil))
	     (format t "Skipping ~a:~%  ~a~%" p c))))))
   source-directory)

  (when (and move-images (not *no-execute*))
    (map-over-directory
     (lambda (p) (warn "Source file: ~a" p))
     source-directory)))

(defun exif-based-name (file output-directory camera file-date)
  (let* ((exif-info (parse-exif-data file))
	 (raw-camera (exif-info-model exif-info))
	 (camera (if* camera
		    then camera
		  elseif (string= "Canon EOS D30" raw-camera)
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
		  elseif (string= "Palm Centro" raw-camera)
		    then "Centro"
		  elseif (string= "Canon PowerShot S1 IS" raw-camera)
		    then "S1IS"
		  elseif (string= "Canon PowerShot S20" raw-camera)
		    then "S20"
		  elseif (string= "Canon PowerShot S30" raw-camera)
		    then "S30"
		  elseif (string= "Canon PowerShot S40" raw-camera)
		    then "S40"
		  elseif (string= "Canon PowerShot G1" raw-camera)
		    then "G1"
		  elseif (string= "Canon PowerShot G2" raw-camera)
		    then "G2"
		  elseif (string= "DMC-LX3" raw-camera)
		    then "LX3"
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
	(if* (and (null (exif-info-date exif-info)) file-date)
	   then (list 0 nil nil nil nil nil)
	   else (mapcar #'read-from-string
			(split-regexp
			 (load-time-value (compile-regexp "[: ]"))
			 (or (exif-info-date exif-info)
			     (error "No :date in exif info.")))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; utils

(defun error-die (format &rest args)
  (if* *debug*
     then (apply #'break format args)
     else (fresh-line t)
	  (apply #'format t format args)
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
