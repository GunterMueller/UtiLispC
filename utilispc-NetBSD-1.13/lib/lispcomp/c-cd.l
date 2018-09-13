(comment "%W% %G%")

(comment defnprop getd
   (lambda (args loc jmp)
    (lets ((a (c:get-cvar)) (d (c:get-cvar)))
     (cond ((= (length args) 1)
	    `(begin
	       ,(c:form (first args) d nil)
	       ,(c:check-sym (first args) d)
	       (= ,a (definition ,d))
	       ,(and udfcheck `(if (spec ,a) (udferr1 ,d fp)))
	       ,(c:final a loc jmp)))
	   (t (c:argerr args)))))
   inline)

(defnprop putd
   (lambda (args loc jmp)
    (match args
      ((('quote sym) ('function ('lambda formals . body)))
       (c:compile-function sym formals body hstream cstream)
       (c:make-code (c:post-incr ncodepieces) loc jmp))
      ((('quote sym) ('function ('macro 'lambda formals . body)))
       (c:compile-macro sym formals body hstream cstream)
       (c:make-code (c:post-incr ncodepieces) loc jmp))
      ((sy def)
       (lets ((r1) (r2) (nsp))
        `(begin ,(c:eval-twoargs (first args) (second args))
	        ,(c:check-sym (first args) (c:value r1))
	        ,(c:assign `(definition ,(c:value r1)) r2)
	        ,(c:final r2 loc jmp))))
      (t (c:argerr args))))
   inline)

(defun c:make-code (n loc jmp)
   (cond ((eq current-fn '*toplevel*)
	  (c:final `(make_code ,n (dis ,sp)) loc jmp))
	 (t (lets ((lit (c:push-literal 0)))
	     `(begin (if (== ,lit (itofix 0))
		         ,(c:assign lit `(make_code ,n (dis ,sp))))
		     ,(c:final lit loc jmp))))))
	      
(defnprop make-undefined
   (lambda (args loc jmp)
    (lets ((a (c:get-cvar)))
     (cond ((= (length args) 1)
	    `(begin
	       ,(c:form (first args) a nil)
	       ,(c:check-sym (first args) a)
	       (= (definition ,a) uderr)
	       ,(c:final a loc jmp)))
	   (t (c:argerr args)))))
   inline)

(comment defnprop definedp
   (lambda (args loc jmp)
    (lets ((a (c:get-cvar)))
     (cond ((= (length args) 1)
	    `(begin
	       ,(c:form (first args) a nil)
	       ,(c:check-sym (first args) a)
	       (= ,a (definition ,a))
	       (if (spec ,a) ,(c:symbol nil loc jmp) ,(c:symbol t loc jmp))))
	   (t (c:argerr args)))))
   inline)

(defnprop specialp
   (lambda (args loc jmp)
    (lets ((a (c:get-cvar)))
     (cond ((= (length args) 1)
	    `(begin
	       ,(c:form (first args) a nil)
	       ,(c:check-sym (first args) a)
	       (= ,a (definition ,a))
	       (if (&& (!= ,a uderr) (spec ,a))
		   ,(c:symbol t loc jmp)
		   ,(c:symbol nil loc jmp))))
	   (t (c:argerr args)))))
   inline)

(defnprop putprop
   (lambda (args loc jmp)
    (match args
       ((('quote sym) ('function ('lambda formals . body)) ('quote name))
        (c:compile-prop (gensym "prop") formals body sym name hstream cstream)
        (c:make-code (c:post-incr ncodepieces) loc jmp))
       ((('quote sym) ('quote ('lambda formals . body)) ('quote name))
        (c:compile-prop (gensym "prop") formals body sym name hstream cstream)
        (c:make-code (c:post-incr ncodepieces) loc jmp))
       ((sym value name) (c:call-sy 'putprop args loc jmp))
       (t (c:argerr args))))
   inline)
