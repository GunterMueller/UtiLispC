;;; -*- Mode: Lisp -*-
;;; script for loading ULX (UtiLisp X interface library)
;;;
(defun load (filename (show))
  (cond ((or (eq (sref filename 0) (character "/"))
	     (eq (sref filename 0) (character "."))))
	(t (setq filename (string-append system_lib_path filename))))
  (princ filename)(terpri)
  (exfile filename show))

(code-load "./socket.o")

;(load "lisplib/readmacro.l")
(load "lisplib/common.l")
(load "lisplib/struct.l")
(load "lisplib/defun.l")
;(load "./lisplib/deftype.l")

(load "./defsetf.l")
(load "./commonlib.l")




(load "./longarith.l")
(load "./depdefs.l")
(load "./dependent.l")
(load "./display.l")
(load "./buffer.l")
(load "./bufmac.l")
(load "./ulx.l")
(load "./macros.l")
(load "./input.l")
(load "./requests.l")
(load "./graphics.l")
(load "./gcontext.l")
(load "./attributes.l")
(load "./fonts.l")
(load "./manager.l")
(load "./text.l")
(load "./translate.l")
(load "./ulxlib.l")

