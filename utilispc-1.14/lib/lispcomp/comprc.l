(setq cfs
  '("comp.o" "c-cd.o" "c-fix.o" "c-list.o" "c-map.o" "c-pred.o" "c-spec.o"
    "dump.o" "compile.o" "opt.o"))
(defun compload (d x)
   (code-load (mapcar x (function (lambda (y) (string-append d y))))))
(compload (string-append system_lib_path "lispcomp/") cfs)
;(exfile (string-append system_lib_path "lispcomp/compile.l"))
