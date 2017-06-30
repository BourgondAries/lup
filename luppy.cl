;; We use this struct to create the FFI
;;
;; From inotify.h
;;
;struct inotify_event {
;  __s32   wd;   /* watch descriptor */
;  __u32   mask;   /* watch mask */
;  __u32   cookie;   /* cookie to synchronize two events */
;  __u32   len;    /* length [including nulls] of name */
;  char    name(0);  /* stub for possible name */
;};
(defparameter IN_ACCESS         #x00000001)  ; File was accessed
(defparameter IN_MODIFY         #x00000002)  ; File was modified
(defparameter IN_ATTRIB         #x00000004)  ; Metadata changed
(defparameter IN_CLOSE_WRITE    #x00000008)  ; Writtable file was closed
(defparameter IN_CLOSE_NOWRITE  #x00000010)  ; Unwrittable file closed
(defparameter IN_OPEN           #x00000020)  ; File was opened
(defparameter IN_MOVED_FROM     #x00000040)  ; File was moved from X
(defparameter IN_MOVED_TO       #x00000080)  ; File was moved to Y
(defparameter IN_CREATE         #x00000100)  ; Subfile was created
(defparameter IN_DELETE         #x00000200)  ; Subfile was deleted
(defparameter IN_DELETE_SELF    #x00000400)  ; Self was deleted
;; the following are legal events.  they are sent as needed to any watch
(defparameter IN_UNMOUNT        #x00002000)  ; Backing fs was unmounted
(defparameter IN_Q_OVERFLOW     #x00004000)  ; Event queued overflowed
(defparameter IN_IGNORED        #x00008000)  ; File was ignored
;; helper events
(defparameter IN_CLOSE          (logior IN_CLOSE_WRITE IN_CLOSE_NOWRITE)) ; close
(defparameter IN_MOVE           (logior IN_MOVED_FROM IN_MOVED_TO)) ; moves
;; special flags
(defparameter IN_ISDIR          #x40000000)  ; event occurred against dir
(defparameter IN_ONESHOT        #x80000000)  ; only send event once
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TODO implement checking of shared objs
(load-shared-object "libnotify.so.4")
; (load-shared-object "libinotifytools.so")
(ql:quickload "cffi")
(ql:quickload :cl-fad)
(ql:quickload :cl-ppcre)
(defparameter *lups* (make-hash-table))
(defparameter *name-buffer-len* 4096)
(defparameter *sizeof-inotify-event* (+ 4 4 4 4 *name-buffer-len*))
(define-alien-type nil
  (struct inotify_event (wd int)
                        (mask int)
                        (cookie sb-alien:unsigned-int)
                        (len sb-alien:unsigned-int)
                        (name (array char 4096))))
(define-alien-routine "inotify_init" int)
(define-alien-routine "inotify_add_watch" int
                      (fd int)
                      (pathname c-string)
                      (mask unsigned-int))
(define-alien-routine "inotify_rm_watch" int
                      (fd int)
                      (wd int))
(define-alien-routine ("read" readwatch) size-t
                      (fildes int)
                      (bufe (* (struct inotify_event)))
                      (offset size-t))
(defparameter *inotify* (inotify-init))
(defun wait-for-event ()
  (with-alien ((res (struct inotify_event)))
    (dotimes (i *name-buffer-len*)
      (setf (deref (slot res 'name) i) 0))
    (readwatch *inotify* (addr res) *sizeof-inotify-event*)
    (list (slot res 'wd) (cast (slot res 'name) c-string))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list-all-files-in-directory-that-match (directory regex)
  (let ((final '()))
    (dolist (entry (cl-fad:list-directory directory) final)
      (when (ppcre:all-matches-as-strings (concatenate 'string "^" regex "$") (file-namestring entry))
        (push entry final)))
    final))
(defun for-each-in-filesystem (on-file on-dir dir)
  (dolist (entry (cl-fad:list-directory dir))
      (unless (equal (char (enough-namestring entry) 0) #\.)
          (if (cl-fad:directory-pathname-p entry)
            (progn
              (funcall on-dir entry)
              (for-each-in-filesystem on-file on-dir entry))
            (funcall on-file entry)))))
(defun on-file (x)
  ;; Allow arbitrary programs to be run, we do an strace on those to get all the opens and writes from them
  ;; Then once we have the complete list of those we add a watch for each of those files. For generated
  ;; files we have IN_CLOSE_WRITE and IN_DELETE and for read files we have the same. Each such even MUST re-run the
  ;; program. The program must be stored in a table from file to program, so a file update triggers the program.
  ;; This part is actually the ONLY important part, we watch ALL files that conform to some regex. Ok fine.
  ;; Now what's next?
  ;; We're watching all the lup.cl files. We've stored them in some list
  ;; Now we execute each and monitor the strace. For each entry in the strace, add a watch, but only if it's inside
  ;; our directory tree. Ok fine, now we have
  ;; (lup.cl, d/e/lup.cl, f/8/o/lup.cl, ...), the deepest should be executed first
  ;; and
  ;; ((3 (lup.cl d/e/lup.cl ...)) (8 (lup/lup.cl bla/bash.sh)))
  ;; Now every time a watch changes, we look it up in the above hash and find which lups to run.
  ;; What happens if a lup depends on the result of another lup? Well we fail and try another lup
  ;; We'd need two working lists, one containing the non-completed lups and the other containing the complete lups.
  ;; The non completed list rotates, so any target that does not compile is pushed to the back of the list.
  ;; Here's the cool thing; we assemble a list of targets that can be compiled in one go, because the succesful ones
  ;; are placed iteratively. Of course side-effects can occur but assume they don't.
  ;; Ok so we have this list, we run each program, now we have more shit to watch for, so we watch everything that strace shows
  ;; Cool. Much better than watching EVERYTHING, well except if need to watch for new builders, so all directories must be watched
  ;;
  (print "ON FILE")
  (print x)
    (when (ppcre:all-matches-as-strings "^builder$" (file-namestring x))
          (let ((watch (inotify-add-watch *inotify* (namestring x) (logior IN_CLOSE_WRITE IN_CREATE IN_DELETE IN_DELETE_SELF))))
               (setf (gethash watch *lups*) (list (namestring x))))))
;; Add a directory to the watches.
(defun on-dir (x)
    (unless (equal (aref (enough-namestring x) 0) #\.)
            (print (enough-namestring x))
            (let ((watch (inotify-add-watch *inotify* (namestring x) (logior IN_CREATE IN_DELETE IN_DELETE_SELF IN_MOVED_FROM IN_MOVED_TO))))
                 (ensure-directories-exist (concatenate 'string ".output/" (enough-namestring x)))
                 (setf (gethash watch *lups*) (list (namestring x))))))
;; Run the command, trace it, and insert files back into *lups*
(defun balgo (pending)
  (when (> (length pending) 0)
      (let ((cmd (first pending)))
          (print (file-namestring cmd))
          (if (zerop (process-exit-code (run-program "/bin/bash" (list "-c" (concatenate 'string "cd d && ./" (file-namestring cmd)))
                                                     :input nil :output *standard-output*)))
            (balgo (rest pending))
            (balgo (rest pending))))))
;; Set up watches for all REGEX files -> builders
;; Set up watches for all directories -> observers
;; Run all builders in -> Balgo (Build Algorithm)
;;
(for-each-in-filesystem #'on-file #'on-dir "")
(merge-pathnames "a/b/c" "d/e/f")

(loop
  (let ((info (wait-for-event)))
       (print info)
       (print (cadr info))
       (print (car (gethash (car info) *lups*)))
       ;; Use merging pathnames later
       (let ((entry (concatenate 'string (car (gethash (car info) *lups*)) "/" (cadr info))))
          (if (cl-fad:directory-exists-p entry)
            (on-dir entry)
            (on-file entry)))
       (print "")))

*lups*

(loop for value being the hash-values of *lups* do
  (unless (cl-fad:directory-exists-p (first value))
    (balgo (list (first value)))))
