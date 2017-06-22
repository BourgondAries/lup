; (ql:system-apropos "vecto")
; (ql:quickload "vecto") ; To load actual software
; (ql:uninstall "name") ; Obvious
; (ql:add-to-init-file)
; (ql:who-depends-on "bla")
; (ql:update-all-dists)
; (ql:update-client)

;; From inotify.h
;struct inotify_event {
;  __s32   wd;   /* watch descriptor */
;  __u32   mask;   /* watch mask */
;  __u32   cookie;   /* cookie to synchronize two events */
;  __u32   len;    /* length [including nulls] of name */
;  char    name(0);  /* stub for possible name */
;};

(defparameter IN_ACCESS   #x00000001)
(defparameter IN_MODIFY   #x00000002)
(defparameter IN_ATTRIB   #x00000004)
(defparameter IN_CLOSE_WRITE    #x00000008)
(defparameter IN_CLOSE_NOWRITE  #x00000010)
(defparameter IN_OPEN     #x00000020)
(defparameter IN_MOVED_FROM   #x00000040)
(defparameter IN_MOVED_TO   #x00000080)
(defparameter IN_CREATE   #x00000100)
(defparameter IN_DELETE   #x00000200)
(defparameter IN_DELETE_SELF    #x00000400)
;/* the following are legal events.  they are sent as needed to any watch */
;#define IN_UNMOUNT    0x00002000  /* Backing fs was unmounted */
;#define IN_Q_OVERFLOW   0x00004000  /* Event queued overflowed */
;#define IN_IGNORED    0x00008000  /* File was ignored */
;
;/* helper events */
;#define IN_CLOSE    [IN_CLOSE_WRITE | IN_CLOSE_NOWRITE] /* close */
;#define IN_MOVE     [IN_MOVED_FROM | IN_MOVED_TO] /* moves */
;
;/* special flags */
;#define IN_ISDIR    0x40000000  /* event occurred against dir */
;#define IN_ONESHOT    0x80000000  /* only send event once */
;; end inotify.h
;(quicklisp-quickstart:install)
;(ql:add-to-init-file)
; TODO implement checking of shared objs
;(load-shared-object "libnotify.so")
(load-shared-object "libinotifytools.so")
(ql:quickload :cl-fad)
(ql:quickload :cl-ppcre)

(ppcre:all-matches-as-strings "^output/.*$" ".output/kek/ura")

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
    ;(print (slot res 'wd))
    ;(print (slot res 'mask))
    ;(print (slot res 'cookie))
    ;(print (slot res 'len))
    ;(print (cast (slot res 'name) c-string))
    (list (slot res 'wd) (cast (slot res 'name) c-string))
    ))
(defun read-from-file (pathname-designator)
  (with-open-file (s pathname-designator)
    (let* ((result (read s))
           (eof-result (cons nil nil))
           (after-result (read s nil eof-result)))
      (unless (eq after-result eof-result)
        (error "more than one expression in file ~S" pathname-designator))
      result)))
(compile 'read-from-file)
; [setf [gethash 'one-entry *my-hash*] "one"]

(defun list-all-files-in-directory-that-match (directory regex)
  (let ((final '()))
    (dolist (entry (cl-fad:list-directory directory) final)
      (when (ppcre:all-matches-as-strings (concatenate 'string "^" regex "$") (file-namestring entry))
        (push entry final)))
    final))
(defun for-each-in-filesystem (fn on-dir directory &key (depth-first-p t))
  (dolist (entry (cl-fad:list-directory directory))
    (unless depth-first-p
      (funcall fn entry))
    (when (cl-fad:directory-pathname-p entry)
      (unless (equal (enough-namestring entry) "output/")
        (funcall on-dir entry)
        (for-each-in-filesystem fn on-dir entry)))
    (when depth-first-p
      (funcall fn entry))))

(for-each-in-filesystem (lambda (x) (when (equal (file-namestring x) "lup.cl")
                                          (let ((watch (inotify-add-watch *inotify* (namestring x) (logior IN_CLOSE_WRITE IN_CREATE IN_DELETE IN_DELETE_SELF))))
                                               (setf (gethash watch *lups*) (namestring x)))
                                          (dolist (rule (read-from-file x))
                                            (cond
                                              ((eq (first rule) 'single)
                                               (print "HOHOHO")
                                               (print (list-all-files-in-directory-that-match (directory-namestring (enough-namestring x)) (second rule)))
                                               (print "HAHAHA"))
                                              ((eq (first rule) 'batch) (print "BATCH")))
                                            (print rule))
                                    ))
                        (lambda (x) (unless (equal (aref (enough-namestring x) 0) #\.)
                                    (print (enough-namestring x))
                                    (let ((watch (inotify-add-watch *inotify* (namestring x) (logior IN_CREATE IN_DELETE IN_DELETE_SELF IN_MOVED_FROM IN_MOVED_TO))))
                                         (ensure-directories-exist (concatenate 'string "output/" (enough-namestring x)))
                                         (setf (gethash watch *lups*) (namestring x)))))
                        "")

(sb-ext:run-program "/bin/bash" (list "-c" "ls") :input nil :output *standard-output*)

(loop
  (let ((info (wait-for-event)))
       (print info)
       (print (gethash (car info) *lups*))
       (print "")))
