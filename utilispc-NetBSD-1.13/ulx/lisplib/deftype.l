;;; Common Lisp deftype emulation (incomplete).
;;;
;;; Author: Kenji Nagahashi.
;;;
;;; $Revision: 1.1.1.1 $
;;; $Log: deftype.l,v $
;;; Revision 1.1.1.1  2000/09/26 00:51:09  ktanaka
;;;
;;;
;;; Revision 1.1  1992/03/19  06:08:19  ken
;;; Initial revision
;;; 

;;; Common Lisp no deftype
;;;
;;; (deftype <type-symbol> (<args>) <type-specification>)
;;; <type-specification ha symbol ka list de, quote ga hituyou dearu.
;;; nakade <args> wo sanshou sitemo yoi (kono baai backquote notation wo
;;; tukau kotoni naru.
;;;
;;; example:
;;;   (deftype buffer-bytes () '(simple-array (unsigned-byte 8) (*)))
;;;   (deftype square-array (n) `(simple-array * (,n ,n)))
;;;
;;; kono emulation code deha <type-specification> ga symbol no baai
;;; sika toriatukatte orazu, list de aru baai niha sono saishono youso
;;; (= car) dake wo miteiru.  dakara backquote notation ha tukaenaisi,
;;; ue no example no youni array no youso wo gentei suru toka, souiu kotomo
;;; dekinai.  deftype no nakade deftype de teigi sita type-symbol wo
;;; tukau koto ha support siteiru.
;;;
;;; mouchotto Common Lisp rasiku
;;;   (simple-array (unsigned-byte 8) (*)) -> string.
;;;   (unsigned-byte 16) -> fixnum.
;;; gurai no handan ha sita houga benri da.
;;;
(defmacro deftype (symbol argl defbody)
  (when argl (err:argument-type argl 'funcall))
  `(progn 
     (putprop ',symbol ',defbody 'sys::deftype-spec)
     (defun ,(intern (string-append symbol '-p)) (object)
       (typep object ',symbol))))

(defmacro defpredicate (type predicate)
  `(putprop ',type ,predicate 'sys::type-predicate))

;; utilisp basic types
(defpredicate integer #'intergerp)
(defpredicate atom #'atom)
(defpredicate vector #'vectorp)
(defpredicate string #'stringp)
(defpredicate bignum #'bigp)
(defpredicate fixnum #'fixp)
(defpredicate null #'null)
(defpredicate symbol #'symbolp)
(defpredicate number #'numberp)
(defpredicate list #'listp)
(defpredicate cons #'consp)

;; Common Lisp basic types
(defpredicate integer #'numberp)
(defpredicate signed-byte #'numberp)
(defpredicate unsigned-byte #'numberp)
(defpredicate simple-vector #'vecotrp)

(defun typep:atom (object type)
  (let ((pred (get type 'sys::type-predicate))
	(spec (get type 'sys::deftype-spec)))
    (cond (pred (funcall pred object))
	  (spec (typep object spec))
	  (t (err:argument-type type 'unknown-type-name)))))

(defun typep (object type)
  (cond ((or (eq type t) (eq type nil)) type)
	((atom type) (typep:atom object type))
	(t
	 (let ((type (car type))
	       (rest (cdr type)))
	   ;; backquote notation wo tukaeru youni suru nara,
	   ;; kokoni type wo eval suru youni kaenakute ha ikenai.
	   (selectq type
	     (quote (typep object (car rest)))
	     (or (some rest #'(lambda (t) (typep object t))))
	     (and (every rest #'(lambda (t) (typep object t))))
	     (not (not (typep object (car rest))))
	     (member (memq object rest))
	     ;; CL furikko
	     ;; restriction ha issai kanchi sinai toiu...
	     (t (typep object type)))))))

(defmacro typecase (val . spec)
  `(let ((@ ,val))
     (cond . ,(mapcar spec
		      #'(lambda (spec1)
			  (selectq (car spec1)
			    (otherwise `(t . ,(cdr spec1)))
			    (t `((typep @ ',(car spec1))
				 . ,(cdr spec1))))))
	     )))

(defmacro etypecase (val . spec)
  `(let ((@ ,val))
     (cond
      . ,(append
	  (mapcar spec
		  #'(lambda (spec1)
		      (selectq (car spec1)
			(otherwise (err:argument-type spec 'no-otherwise))
			(t `((typep @ ',(car spec1))
			     . ,(cdr spec1))))))
	  '((t (err:argument-type @ 'etypecase-bad-val)))))))


