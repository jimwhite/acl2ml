("/home/acl2/books/misc/trace-star.lisp::TRACE*-ENTRY"
 (NIL NIL (SB-INT:QUASIQUOTE) NIL NIL NIL NIL) (NIL NIL (:FMT) NIL NIL NIL NIL)
 (NIL NIL NIL NIL NIL (MSG) NIL)
 (("~@1~y2") NIL (FIRST-TRACE-PRINTING-COLUMN) (CONS) NIL (COND) NIL)
 ((STATE) NIL
  ((< 1 (@ TRACE-LEVEL)) (NOT (EQ ':NONE (@ GUARD-CHECKING-ON)))
   (GETPROP ',FN 'PREDEFINED NIL 'CURRENT-ACL2-WORLD (W STATE)) T QUOTE)
  (MAKE-EVALABLE-WITH-STOBJS) NIL NIL NIL)
 (("" "!! Warning: guard-checking is not :none, so trace    !!~|~t0~
                 !!   output could be misleading or appear incorrect. !!~|~t0~
                 !!   (see :DOC set-guard-checking)                   !!~|~t0"
   "!! Warning: tracing a built-in function, so trace    !!~|~t0~
                 !!   output could be misleading or appear incorrect. !!~|~t0~
                 !! (Consider writing & using a wrapper function.)    !!~|~t0"
   "" ,FN ARGLIST)
  NIL NIL NIL NIL NIL (GETPROP))
 ((NIL) NIL (QUOTE QUOTE QUOTE W) NIL NIL NIL NIL))
("/home/acl2/books/misc/trace-star.lisp::TRACE*-EXIT"
 (NIL NIL (SB-INT:QUASIQUOTE) NIL NIL NIL NIL) (NIL NIL (:FMT) NIL NIL NIL NIL)
 (NIL NIL NIL NIL NIL (MSG) NIL)
 (("~y2~|~t0= ~y1") NIL NIL (MAX LET CONS) NIL NIL NIL)
 (((STOBJS-OUT
    (GETPROP ',FN 'STOBJS-OUT '(NIL) 'CURRENT-ACL2-WORLD (W STATE))))
  (0) (QUOTE) (- MAKE-EVALABLE-WITH-STOBJS) (IF) NIL NIL)
 ((,FN ARGLIST) (2) (FIRST-TRACE-PRINTING-COLUMN CAR) (AND CONS) NIL NIL
  (GETPROP))
 ((STATE NIL) NIL (CONSP ENDP QUOTE QUOTE QUOTE QUOTE W)
  (MAKE-EVALABLE-WITH-STOBJS MAKE-EVALABLE-WITH-STOBJS) NIL NIL NIL))
("/home/acl2/books/misc/trace-star.lisp::TRACE*-MODIFY1"
 (NIL NIL NIL NIL (COND) NIL NIL)
 (NIL NIL
  ((AND (CONSP TRACE-SPEC) (SYMBOLP (CAR TRACE-SPEC))
        (KEYWORD-VALUE-LISTP (CDR TRACE-SPEC)))
   (SYMBOLP TRACE-SPEC) T)
  NIL NIL NIL NIL)
 (NIL NIL NIL (LET ACL2ML-COMPLETE-ORIGINAL::RECURSIVE-CALL) NIL (ER) NIL)
 (((FN (CAR TRACE-SPEC)) CTX HARD CTX
   "A trace spec must be a symbol or a symbol consed onto an alternating list ~
            of the form (:kwd1 val1 :kwd2 val2 ...).  The trace spec ~x0 is thus ~
            illegal.  See :DOC trace$."
   TRACE-SPEC)
  NIL (LIST) (APPEND) NIL NIL NIL)
 ((TRACE-SPEC TRACE-SPEC) NIL NIL NIL NIL NIL NIL)
 ((:ENTRY :EXIT :HIDE NIL :EVISC-TUPLE) NIL (TRACE*-ENTRY TRACE*-EXIT QUOTE)
  NIL NIL NIL NIL)
 ((FN FN) NIL NIL NIL NIL (LIST) NIL))
("/home/acl2/books/misc/trace-star.lisp::TRACE*-MODIFY"
 (NIL NIL NIL (COND) NIL NIL NIL)
 (NIL NIL ((ENDP TRACE-SPECS) T) NIL NIL NIL NIL)
 ((NIL) NIL NIL (CONS) NIL NIL NIL)
 (NIL NIL NIL (TRACE*-MODIFY1 ACL2ML-COMPLETE-ORIGINAL::RECURSIVE-CALL) NIL NIL
  NIL)
 ((CTX CTX) NIL (CAR CDR) NIL NIL NIL NIL)
 ((TRACE-SPECS TRACE-SPECS) NIL NIL NIL NIL NIL NIL)
 (NIL NIL NIL NIL NIL NIL NIL))
