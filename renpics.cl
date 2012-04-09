;; Rename picture files based on date they were taken.
;;
;; This software is Copyright (c) Kevin Layer, 2000-2005.
;; Kevin Layer grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)

(eval-when (compile)
  (compile-file-if-needed "exif-utils/exifinfo.cl"))

(eval-when (compile eval load)
  (require :exifinfo "exif-utils/exifinfo.fasl")
  (use-package :util.exif)
  
  (require :regexp2)

  (require :aclwin)
  (require :fileutil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; variables that change with the addition of new cameras:

(defparameter *types*
    '((:movie "mov"  #+ignore ("jpg" "thm")) ;; 5D3 has no companion...
      (:movie "avi"  ("jpg" "thm"))
      (:movie "mts"  nil)
      (:image "jpg"  ("wav" "cr2"))
      (:image "jpeg" ("wav" "cr2"))
      ;; All from the Panasonic TS3
      (:ignore "mpl")
      (:ignore "bdm")
      (:ignore "cpi")
      (:ignore "tid")
      (:ignore "tdt")
      ))

(defparameter *camera-string-to-abbrev*
    `(("Canon EOS 5D Mark III" . "5D3")
      ("Canon EOS 7D" . "7D")
      ("Canon EOS D30" . "D30")
      ("Canon EOS 5D Mark II" . "5D2")
      ("Canon EOS 1D" . "1D")
      ("Galaxy Nexus" . "GN")
      ("DSC-D700" . "D700")
      ("Palm Centro" . "Centro")
      ("Canon PowerShot S1 IS" . "S1IS")
      ("Canon PowerShot S20" . "S20")
      ("Canon PowerShot S30" . "S30")
      ("Canon PowerShot S40" . "S40")
      ("Canon PowerShot G1" . "G1")
      ("Canon PowerShot G2" . "G2")
      ("DMC-LX3" . "LX3")
      ("DMC-TS3" . "TS3")
      (,(lambda (raw-camera file)
	  (when
	      (or (string= "E950" raw-camera)
		  ;; hack for me:
		  (match-re
		   (load-time-value (compile-re "^nikon"))
		   (file-namestring file)
		   :case-fold t :return nil))
	    "N950")))
      (,(lambda (raw-camera file)
	  (declare (ignore file))
	  (when (member raw-camera '("E990" "E800" "E700")
			:test 'string=)
	    raw-camera)))
      (,(lambda (raw-camera file)
	  (declare (ignore file))
	  (when (match-re
		 (load-time-value (compile-re "nikon d1"))
		 raw-camera
		 :case-fold t :return nil)
	    "D1")))
      (,(lambda (raw-camera file)
	  (when (or (match-re
		     (load-time-value (compile-re "^dc210"))
		     raw-camera
		     :case-fold t :return nil)
		    ;; hack for me:
		    (match-re
		     (load-time-value (compile-re "^dcp"))
		     (file-namestring file)
		     :case-fold t :return nil))
	    "DC210")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *quiet* nil)
(defvar *no-execute* nil)
(defvar *debug* nil)
(defvar *usage*
    "Usage: [-c camera] [-f] [-m] [-n] [-q] [-o output-directory] directory
-c camera
   :: force camera name to `camera' -- useful if the camera is not one of
      the known types
-f :: do *NOT* use file write date if there is no exif information in file
-m :: move, instead of copy, images to destination directory
-n :: just print what would be done, but do not do it
-q :: operate quietly
-o output-directory
   :: send files to output-directory -- defaults to 'directory'
directory
   :: the location of the pictures to rename

renpics looks inside the camera files at the EXIF info and pulls
out the date and camera name.  The camera name is assigned an
abbreviation.  The final name of the file has this format:

  YYYYMMDD-hhmm-ssnn-xxxx.jpg

where YYYY is the year, MM is the month, DD is the day, hh is the hour, mm
is the minute, ss is the second, nn is a sequence number used only when
more than one picture was taken in a given second, and xxxx is the camera
abbreviation based on the real name, or that set with the -c argument.
")

(defun main ()
  (handler-case
      (sys:with-command-line-arguments
	  ("c:fo:mnq"
	   camera no-file-date output-directory move
	   *no-execute* *quiet*)
	  (rest)
	(when (/= 1 (length rest))
	  (format t "~&You must provide an input directory.~%")
	  (error-die *usage*))
	(renpics (first rest) output-directory :move move
		 :camera camera :file-date (not no-file-date))
	(exit 0 :quiet t))
    (error (c) (error-die "An error occurred: ~a." c))))

(defun find-type (p)
  (dolist (type *types*)
    (when (equalp (second type)
		  (pathname-type p))
      (return (values-list type)))))

(defstruct fileinfo
  path					; the path to the file
  exif-file				; the file with the exif info
  companions				; a list of companions
  type					; type (:image or :movie)
  )

(defun find-companions (p types &aux (res '()) temp)
  (dolist (type types)
    (when (probe-file
	   (setq temp
	     (merge-pathnames (make-pathname :type type)
			      p)))
      (push temp res)))
  ;; preserve order from `types':
  (nreverse res))

(defun note-type (ht type files)
  (when files
    (when (not (consp files))
      (setq files (list files)))
    (dolist (file files)
      (setf (gethash (file-namestring file) ht)
	type))))

(defun renpics (source-directory output-directory
		&key camera (file-date t) move
		     ((:no-execute *no-execute*) *no-execute*)
		&aux (nimages 0)
		     (nmovies 0)
		     (images '())
		     (movies '())
		     (unknown '())
		     (ht
		      ;; make file-namestring to type.  Values are:
		      ;;   :image, :movie or :companion'
		      (make-hash-table :size 777 :test #'equalp)))
  (when (not (probe-file source-directory))
    (error-die "Directory ~a does not exist." source-directory))
  
  ;; Movies first, since their types overlap with images.
  (map-over-directory
   (lambda (p)
     (when (not (gethash (file-namestring p) ht))
       ;; We've not seen this file before...
       (multiple-value-bind (type file-type companion-types) (find-type p)
	 (when (and (eq :movie type)
		    (equalp file-type (pathname-type p)))
	   (let ((companions (find-companions p companion-types)))
	     (if* companions
		then (note-type ht :companion companions)
	      elseif companion-types
		then (error "There was no companion for movie: ~a." p))
	     (note-type ht :movie p)
	     (push (make-fileinfo :type :movie :path p
				  :exif-file (or (car companions) p)
				  :companions companions)
		   movies)
	     (incf nmovies))))))
   source-directory)
  
  (map-over-directory
   (lambda (p)
     (block done
       (when (not (gethash (file-namestring p) ht))
	 ;; We've not seen this file before...
	 (multiple-value-bind (type file-type companion-types) (find-type p)
	   (when (equalp file-type (pathname-type p))
	     (when (eq :ignore type) (return-from done))
	     (when (eq :movie type)
	       (error "unexpected movie type: ~s, ~s." type p))
	     (when (eq :image type)
	       (let ((companions (find-companions p companion-types)))
		 (note-type ht :companion companions)
		 (note-type ht :image p)
		 (push (make-fileinfo :type :image :path p :exif-file p :companions companions)
		       images)
		 (incf nimages)
		 (return-from done)))
	     ;; Fall through...
	     (push p unknown))))))
   source-directory)
  
  (dolist (p unknown)
     (when (not (gethash (file-namestring p) ht))
       (warn "Don't know how to handle: ~a." p)))
  
  (when (not *quiet*)
    (when (> nimages 0)
      (format t "~d images~%" nimages))
    (when (> nmovies 0)
      (format t "~d movies~%" nmovies))
    (format t "~%"))

  ;; Process images before movies so we can set the default camera in the
  ;; images phases.
  
  (process-files (nreverse images)
		 :output-directory output-directory
		 :camera camera
		 :file-date file-date
		 :move move
		 :no-execute *no-execute*)
  
  (process-files (nreverse movies)
		 :output-directory output-directory
		 :camera camera
		 :file-date file-date
		 :move move
		 :no-execute *no-execute*)
  
  t)

(defvar *default-camera-abbrev* nil)

(defun process-files (fileinfos
		      &key output-directory camera file-date move
			   ((:no-execute *no-execute*) *no-execute*)
		      &aux (nfile 1)
			   (op (if move "move" "copy"))
			   (pastop (if move "moved" "copied")))
  (when (and output-directory (not *quiet*))
    (format t ";;~@[~* would~] ~a items to ~a:~%" *no-execute* op
	    output-directory))
  
  (dolist (fi fileinfos)
    #+ignore
    (when (and (null (fileinfo-exif-file fi))
	       (null (fileinfo-companions fi)))
      (when (null output-directory)
	(format t "~d: skipping ~a...~%" nfile (fileinfo-path fi))
	(go next))
      
      ;; Simple move/copy
      (let* ((p (fileinfo-path fi))
	     (new-p (merge-pathnames (file-namestring p) output-directory)))

	(when (and output-directory (probe-file new-p))
	  (warn "File exists in output directory (~a)." (file-namestring p))
	  (go next))

	(when (not *quiet*)
	  (format t "~d:~@[~* would~] ~a ~a to ~a~%"
		  nfile *no-execute* op (file-namestring p) new-p))
      
	(when (not *no-execute*)
	  (if* move
	     then (rename-file p new-p)
	     else (sys:copy-file p new-p)))

	(incf nfile)
      
	(go next)))
    
    (multiple-value-bind (new-p sequence error-info)
	(exif-based-name
	 (fileinfo-exif-file fi) output-directory camera file-date
	 :ignore-exif-errors (eq :movie (fileinfo-type fi)))
      (when error-info
	(warn error-info)
	(go next))
      
      (let ((p (fileinfo-path fi))
	    (companions (if* (fileinfo-companions fi)
			   thenret)))
	
	;; output-directory can have 3 distinct values:
	;;   1. nil (use directory of source file)
	;;   2. specified directory != output-directory
	;;   3. specified directory == output-directory
	;; If we run renpics twice with the same parameters, we want to
	;; handle each of the 3 cases gracefully.
	;;
	;; If moving to the same directory, then make sure we wouldn't just
	;; renumber the files.  In this case, warn and skip.
	(if* (and (same-path-p p new-p)
		  (> sequence 0))
	   then ;; Cases (1) and (3) above
		(warn "File already ~a: ~a." pastop (file-namestring p))
		(go next)
	 elseif (and output-directory (> sequence 0))
	   then ;; Case (2) above
		(warn "File exists in output directory (~a)."
		      (file-namestring p))
		(go next))

	;; Check that all destination files do not exist before we start
	;; copying or moving:
	(and (probe-file new-p)
	     (error "internal error: new-p exists: ~s." new-p))
	(dolist (companion companions)
	  (let ((new-companion (merge-pathnames
				(make-pathname :type (pathname-type companion))
				new-p)))
	    (and (probe-file new-companion)
		 (error "internal error: new companion exists: ~s."
			new-companion))))
	
	(when (not *quiet*)
	  (format t "~3d:~@[~* would~] ~a ~a to ~a~%"
		  nfile *no-execute* op (file-namestring p)
		  (file-namestring new-p)))
	
	(when (not *no-execute*)
	  (if* move
	     then (rename-file p new-p)
	     else (sys:copy-file p new-p)))
	(dolist (companion companions)
	  (let ((new-companion (merge-pathnames
				(make-pathname :type (pathname-type companion))
				new-p)))
	    (when (not *quiet*)
	      (format t "     ~a ~a to ~a~%"
		      op (file-namestring companion) (file-namestring new-companion)))
	    (when (not *no-execute*)
	      (if* move
		 then (rename-file companion new-companion)
		 else (sys:copy-file companion new-companion)))))))
   next
    (incf nfile))
  
  (format t "~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; exif-based naming

(defun exif-based-name (file output-directory given-camera file-date
			&key ignore-exif-errors)
  (let* ((exif-info (handler-case (parse-exif-data file)
		      (error (c)
			(when (not ignore-exif-errors)
			  (warn "parsing exif: ~a" c))
			nil)))
	 (raw-camera (when exif-info (exif-info-model exif-info)))
	 camera-abbrev
	 name
	 (sequence 0))
    
    (setq camera-abbrev
      (if* given-camera
	 thenret
       elseif *default-camera-abbrev*
	 thenret
	 else (when (null raw-camera)
		(return-from exif-based-name
		  (values
		   nil nil
		   (format nil "Cannot determine raw camera name from ~a."
			   file))))
	      (or (camera-to-abbrev raw-camera file)
		  (error "Cannot convert raw name to abbreviation: ~a."
			 raw-camera))))
    ;; Set this so renaming movies w/o exif info can include a camera name
    (when (null *default-camera-abbrev*)
      (setq *default-camera-abbrev* camera-abbrev))
    
    (destructuring-bind (year month day hour minute second)
	(if* (and (or (null exif-info)
		      (null (exif-info-date exif-info)))
		  file-date)
	   then (list 0 nil nil nil nil nil)
	   else (mapcar #'read-from-string
			(split-re
			 (load-time-value (compile-re "[: ]"))
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
		  camera-abbrev
		  (pathname-type file)))
	(let ((new (merge-pathnames name
				    (if* output-directory
				       thenret
				       else file))))
	  (when (not (probe-file new))
	    (return-from exif-based-name (values new sequence))))
	(incf sequence)))))

(defun camera-to-abbrev (raw-camera file
			 &aux temp)
  (dolist (item *camera-string-to-abbrev*)
    (if* (functionp (car item))
       then (when (setq temp (funcall (car item) raw-camera file))
	      (return temp))
     elseif (stringp (car item))
       then (when (string= (car item) raw-camera)
	      (return (cdr item)))
       else (error "Bad item car: ~s." (car item)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; utils

(defun error-die (format &rest args)
  (if* *debug*
     then (apply #'break format args)
     else (fresh-line t)
	  (apply #'format t format args)
	  (fresh-line t)
	  (exit 0 :no-unwind t :quiet t)))

#+ignore ;; not sure what this was for, but it's unused now
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

#+ignore ;; not sure what this was for, but it's unused now
(defun read-lines (stream)
  (let ((lines '())
	line)
    (loop
      (setq line (my-read-line stream))
      (when (eq stream line) (return (nreverse lines)))
      (push line lines))))

(defun same-path-p (p1 p2)
  (equalp (path-namestring p1)
	  (path-namestring p2)))

(provide :renpics)

