; (ql:system-apropos "vecto")
; (ql:quickload "vecto") ; To load actual software
; (ql:uninstall "name") ; Obvious
; (ql:add-to-init-file)
; (ql:who-depends-on "bla")
; (ql:update-all-dists)
; (ql:update-client)

(ql:quickload :cl-fad)

(load-shared-object "libnotify.so")
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
                      (bufe c-string)
                      (offset size-t))

(defvar instance (inotify-init))
instance

(defvar wa (inotify-add-watch instance "/home/kstraver/test.cl" 2))
wa

(inotify-rm-watch instance wa)

(readwatch instance "12345678901234567890" 20)

(defun read-from-file (pathname-designator)
  (with-open-file (s pathname-designator)
    (let* ((result (read s))
           (eof-result (cons nil nil))
           (after-result (read s nil eof-result)))
      (unless (eq after-result eof-result)
        (error "more than one expression in file ~S" pathname-designator))
      result)))
(compile 'read-from-file)

(read-file "lup.cl")

(pathspec)

(read-from-file "lup.cl")

(loop (when nil (return))
      (sleep 0.3))


(defun mapc-directory-tree (fn directory &key (depth-first-p t))
  (dolist (entry (cl-fad:list-directory directory))
    (unless depth-first-p
      (funcall fn entry))
    (when (cl-fad:directory-pathname-p entry)
      (mapc-directory-tree fn entry))
    (when depth-first-p
      (funcall fn entry))))

(mapc-directory-tree (lambda (x) (when (equal (pathname-type x) "cl")
                                   (write-line (namestring x))))
                    "stuf/docs/cisco2017/stm/")
