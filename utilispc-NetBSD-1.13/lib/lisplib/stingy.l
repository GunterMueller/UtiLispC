(declare (result) special)
(defun prind-file (file (ll 78) (usebq usebq) (use-lower use-lower))
   (lets ((err:end-of-file (function (lambda (x) (throw 'eof))))
	  (tempfile (string-append "/tmp/" (date-time))))
    (lets ((standard-input (inopen (stream (alloc file)))) 
           (standard-output (outopen (stream (alloc tempfile))))
	   (oldll (getd 'linelength)))
     (putd 'linelength (function (lambda ((x)) 79)))
     (catch 'eof (loop (prind (read)) (terpri)))
     (putd 'linelength oldll)
     (close standard-output))
    (lets ((result nil) (standard-input (inopen (stream tempfile))))
     (catch 'eof (read-tempfile))
     (call (string-append "rm " tempfile))
     (nreverse (cdr result)))))
(defun read-tempfile nil (loop (push (readline) result)))
(defun put-line-number (text (width 5))
   (do ((res) (l text (cdr l)) (lno 1 (/1+ lno)))
       ((null l) (nreverse res))
       (push (string-append (unpack-right lno width) " " (car l)) res)))
(defun title-index (text (width 40))
   (do ((l text (cdr l)) (lno 1 (/1+ lno)) (titles))
       ((null l)
        (setq text (sort titles
                         (function
                          (lambda (x y)
                           (string-lessp (first x) (first y)))))))
       (lets ((title-part (title-part (car l))))
        (and title-part (push `(,title-part ,lno) titles))))
   (do ((l text (cdr l)) (titles))
       ((null l) (reverse titles))
       (lets ((title-part (first (first l))) 
              (lnostr (unpack (second (first l)))) 
              (lnostr-length (string-length lnostr)) (dots-size))
        (setq dots-size (/- width (string-length title-part) lnostr-length 3))
        (cond ((>= dots-size 0)
               (push (string-append
                        title-part
                        "  "
                        (make-string dots-size ".")
                        " "
                        lnostr)
                     titles))
              (t (push (substring title-part 0 (/- width 4)) titles)
                 (setq title-part (substring title-part (/- width 4)))
                 (loop (setq dots-size (/- width (string-length title-part) 
                                        lnostr-length 5))
                       (cond ((>= dots-size 0)
                              (push (string-append
                                       "  "
                                       title-part
                                       "  "
                                       (make-string dots-size ".")
                                       " "
                                       lnostr)
                                    titles)
                              (exit)))
                       (push (string-append
                                "  "
                                (substring
                                   title-part
                                   0
                                   (/- width 5 lnostr-length)))
                             titles)
                       (setq title-part (substring title-part 
                                         (/- width 5 lnostr-length)))))))))
(defun title-part (str)
   (and (not (/0= (string-length str)))
        (not (eq (sref str 0) (character " ")))
        (lets ((i (string-search-char " " str)))
         (and i
              (lets ((header (substring str 0 i)))
               (and (member header
                            '("(defun" "(defmacro" "(macro" "(defprop" 
                              "(setq" "(DEFUN" "(DEFMACRO" "(MACRO" 
                              "(DEFPROP" "(SETQ"))
                    (substring
                       str
                       (/1+ i)
                       (or (string-search-char " " str (/1+ i))
                           (string-length str)))))))))
(defun head-foot (text (title "") (ll 85) (pl 66))
   (lets ((res nil) 
          (titleline
             (string-amend
                (string-amend
                   (make-string ll " ")
                   title
                   (/- ll (string-length title) 2))
                (today)
                2))
          (textlength (/- pl 6)))
    (do ((pno 1 (/1+ pno)))
        ((null text))
        (push "" res)
        (push titleline res)
        (push "" res)
        (do ((i 0 (/1+ i)))
            ((or (= i textlength) (null text))
             (cond ((null text)
                    (do ((j i (/1+ j))) ((= j textlength)) (push "" res)))))
            (push (pop text) res))
        (push "" res)
        (push (string-append
                 (make-string (/- (// ll 2) 3) " ")
                 "- "
                 (unpack pno)
                 " -")
              res)
        (push "" res))
    (nreverse res)))
(defun today nil
   (lets ((date (date-time)))
    (string-append
       (month (pack (substring date 2 4)))
       " "
       (unpack (pack (substring date 4 6)))
       " "
       (substring date 6 8)
       ":"
       (substring date 8 10)
       ", 19"
       (substring date 0 2))))
(defun month (n)
   (nth (/1- n)
        '("Jan." "Feb." "Mar." "Apr." "May" "Jun." "Jul." "Aug." "Sep." 
          "Oct." "Nov." "Dec.")))
(defun unpack (n)
   (cond ((zerop n) "0")
         ((do ((l nil `(,(/+ (character "0") (\ i 10)) . ,l)) (i n (// i 10)))
              ((zerop i)
               (apply 'string-append (mapcar l (function string))))))))
(defun pack (s)
   (do ((n 0 (/+ (* n 10) (/- (sref s i) (character "0"))))
        (i 0 (/1+ i))
        (l (string-length s)))
       ((= i l) n)))
(defun unpack-right (n width)
   (lets ((res (make-string width " ")) (i width))
    (loop (sset res (decr i 1) (/+ (\ n 10) (character "0")))
          (setq n (// n 10))
          (and (/0= n) (exit)))
    res))
(defun twopart (text (pl 66) (ll 174) (center 4))
   (lets ((res) (lefthalf (make-string (// (/+ ll center 1) 2) " ")))
    (do ((page (vector pl "") (fill-vector page "")))
        ((null text))
        (do ((i 0 (/1+ i)))
            ((or (= i pl) (null text))
             (and (null text)
                  (do ((j 0 (/1+ j))) ((= j i)) (push (vref page j) res))))
            (vset page i (pop text)))
        (or (null text)
            (do ((i 0 (/1+ i)))
                ((or (= i pl) (null text))
                 (do ((j i (/1+ j))) ((= j pl)) (push (vref page j) res)))
                (push (string-amend
                         (string-append lefthalf (pop text))
                         (vref page i))
                      res))))
    (nreverse res)))
(defun print-la120 (text (lpi 6) (cpi 13))
;  (linesize 255)
   (progn (comment "SET CHARACTER PITCH")
          (escape-sequence
             "["
             (unpack (cdr (assq cpi
                             '((10 . 1) (12 . 2) (13 . 3) (16 . 4) (5 . 5) 
                               (6 . 6) (7 . 7) (8 . 8)))))
             "w"))
   (progn (comment "SET LINE PITCH")
          (escape-sequence
             "["
             (unpack (cdr (assq lpi
                             '((6 . 1) (8 . 2) (12 . 3) (2 . 4) (3 . 5) 
                               (4 . 6)))))
             "z"))
   (do ((l text (cdr l)))
       ((null l))
       (lets ((i 0) (j) (s (car l)))
        (loop (setq j (string-search-not-char " " s i))
              (cond ((null j) (terpri) (exit)))
              (dec:blanks (/- j i))
              (setq i (string-search-char " " s j))
              (cond ((null i) (princ (substring s j)) (terpri) (exit)))
              (princ (substring s j i))))))
(defmacro dec:blanks (n)
   `(match ,n
       (1 (princ " "))
       (2 (princ "  "))
       (3 (princ "   "))
       (4 (princ "    "))
       (x (escape-sequence "[" (unpack x) "a"))))
(macro escape-sequence (l)
   `(progn (tyo 27) . ,(mapcar l (function (lambda (x) `(princ ,x))))))
(defun make-list (leng (init))
   (do ((i leng (/1- i)) (res nil `(,init . ,res))) ((/0= i) res)))
(defun pad-last-page (text (pl 60))
   (let ((last-page-leng (\ (length text) pl)))
    (cond ((/0= last-page-leng) text)
          (t (nconc text (make-list (/- pl last-page-leng) ""))))))
(defun stingy (file (title file))
   (print-la120
      (twopart
         (head-foot
            (let ((text (prind-file file)))
             (nconc (pad-last-page (put-line-number text))
                    (twopart (title-index text) 60 85)))
            title))))
