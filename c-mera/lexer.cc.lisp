
(include <stdio.h>)
(include <stdlib.h>)
(include <stdint.h>)
(include <string.h>)

;;; Configuration

(defvar ATOM_NUM_NODES 4096) ; Tiene que ser potencia de 2

;;; Utils

(defmacro die (fmt &rest rest)
   `(progn (fprintf stderr ,(format nil "~a~a" fmt "\\n") ,@rest)
           (exit 1)))

;;; Read Whole File

(defmacro open-file (filename file)
   `(progn
      (set ,file (fopen ,filename "r"))
      (if (== ,file 0) (die "Cannot open file '%s'" ,filename))))

(defmacro determine-file-size (file result)
   `(progn
       (fseek ,file 0 SEEK_END)
       (set ,result (cast size_t (ftell ,file)))
       (fseek ,file 0 SEEK_SET)))

(defmacro alloc (var type size)
   `(progn 
      (set ,var (cast ,type (malloc ,size)))
      (if (== ,var 0) (err "Cannot allocate\\n"))))

(defmacro read-all-bytes (file size buffer)
   `(progn 
      (decl ((size_t n-bytes-read))
         (set n-bytes-read (fread (cast void* ,buffer) 1 ,size ,file))
         (printf "Read %lu bytes\\n" n-bytes-read)
         (if (!= n-bytes-read (cast size_t ,size))
            (die "Couldn't read the whole file: read %lu bytes (should be %lu)" 
                 n-bytes-read (cast size_t ,size))))))

(function read-whole-file ((const char* filename)) -> char*
   (macrolet ((err (fmt &rest vars)
                `(progn (funcall fprintf stderr ,fmt ,@vars)
                        (return 0))))
      (decl ((FILE*  file) 
             (size_t size)
             (char*  buffer))
         (open-file filename file)
         (determine-file-size file size)
         (alloc buffer char* size)
         (read-all-bytes file size buffer)      
         (set buffer[n-bytes-read] 0) ; (zero-terminate buffer)
         (return buffer))))

;;; Token list

; Here you can choose what tokens the lexer will handle
; Every token is either:
; 1. a symbol = :if
; 2. a list   = (:end "<eof>")  ; first item is a symbol, second is the associated string
;                               ; the third item is optional and is the size of the token
;                               ; this is normally computed from the string but for "\\\\" should be 1...

(defstruct token kind str len)
(defvar token-table (make-hash-table))

(defun token-atom (sym) (make-symbol (string-upcase (format nil "_ATOM_~a" sym))))

(defmacro deftoken (kind sym str &optional len)
   (lisp (if (null len) (setf len (length str))))
   `(lisp (setf (gethash ,sym token-table) 
                (make-token :kind ,kind :str ,str :len ,len))))

(defmacro defkeyword (keyword)
   `(lisp (setf (gethash ,keyword token-table)
                (make-token :kind :KEYWORD 
                            :str ,(lisp (string-downcase (symbol-name keyword)))
                            :len ,(lisp (length (symbol-name keyword)))))))

(defmacro deftoken-list (kind &rest tokens)
   `(progn ,@(loop for tok in tokens collect `(deftoken ,kind ,@tok))))

(defmacro defkeyword-list (&rest keywords)
   `(progn ,@(loop for kw in keywords collect `(defkeyword ,kw))))

(deftoken :END   :end "<end>" 0)
(deftoken :ERROR :error "<error>" 0)
(deftoken :DIRECTIVE :sharp "#")
(deftoken :BACKSLASH :backslash "\\\\" 1)

(deftoken-list :PUNCT 
   (:colon ";") (:semicolon ";") (:coloncolon "::") (:comma ",") (:qmark "?") (:dot "."))
(deftoken-list :DELIM
   (:lparen "(") (:rparen ")") (:lbracket "[") (:rbracket "]") (:lbrace "{") (:rbrace "}"))
(deftoken-list :OPERATOR
   (:plus "+") (:minus "-") (:star "*") (:div "/") (:mod "%") 
   (:not "!") (:amp "&") (:bar "|") (:xor "^") (:lshift "<<") (:rshift ">>")
   (:eqeq "==") (:le "<=") (:ge ">=") (:lt "<") (:gt ">")
   (:barbar "||") (:ampamp "&&") (:or "or") (:and "and")
   (:arrow "->") (:arrowp "->*")
   (:plusplus "++") (:minusminus "--"))
(deftoken-list :ASSIGN
   (:eq "=") (:stareq "*=") (:minuseq "-=") (:pluseq "+=") (:diveq "/=")
   (:modeq "%=") (:bareq "|=") (:ampeq "&=") (:xoreq "^=") (:barbareq "||=") (:ampampeq "&&=")
   (:noteq "!=")
   (:lshifteq "<<=") (:rshifteq ">>="))

(defkeyword-list :if :else :while :for :switch :case :break :continue :goto :return
     :using :namespace :struct :class :typedef :enum
     :void :int :bool :char :float :double :string
     :short :long :const :unsigned
     :auto :register :static :extern :volatile :mutable
     :inline :virtual :explicit
     :true :false)

;;; Atoms

(enum Kind 
   (:END :ERROR :BACKSLASH :DIRECTIVE :FILENAME
    :USING :PUNCT :DELIM :OPERATOR :ASSIGN :KEYWORD
    :IDENT :CONTROL :TYPEDEF :MODIFIER :TYPE 
    :LIT_BOOL :LIT_INT :LIT_FLOAT :LIT_DOUBLE :LIT_CHAR 
    :LIT_STRING))

(struct Atom
   (decl ((Kind        kind)
          (size_t      len)
          (const char* str))))

(struct Node
   (decl ((Atom  atom)
          (Node* prev))))

(decl ((Node* (array nodes ATOM_NUM_NODES))))

(macrolet ((define-token-atoms ()
              `(decl (,@(loop for sym being the hash-key of token-table
                              collect `(Atom* ,(token-atom sym)))))))
   (define-token-atoms))

(function hash ((const char* p) (size_t len)) -> uint32_t
   (decl ((uint32_t r = 2166136261))
      (for ((uint32_t i = 0) (< i len) i++)
         (set r (^ r *p++))
         (set r (* r 16777619)))
      (return r)))

(defmacro _new (type)
   `(cast (postfix* ,type) (malloc (sizeof ,type))))

(function atom ((Kind kind) (const char *str) (size_t len)) -> Atom*
   (decl ((uint32_t mask = (1- ATOM_NUM_NODES))
          (uint32_t idx  = (& (hash str len) mask))
          (Node*    n))
      (set n (aref nodes idx))
      (while n
         (if (and (== n->atom.len len) (== 0 (strncmp n->atom.str str len)))
            (return (& n->atom)))
         (set n n->prev))
      (set n (_new Node));(cast Node* (malloc (sizeof Node))))
      (set n->atom.kind kind)
      (set n->atom.str str)
      (set n->atom.len len)
      (set n->prev (aref nodes idx))
      (set (aref nodes idx) n)
      (return (& n->atom))))

(function atom ((Kind kind) (const char* str)) -> Atom*
   (return (funcall atom kind str (strlen str))))

(function init () -> void
   (macrolet ((init-token-atoms ()
                  `(progn ,@(loop for sym being the hash-key of token-table
                                 using (hash-value tok)
                                 collect `(set ,(token-atom sym)
                                               (funcall atom ,(token-kind tok)
                                                             ,(token-str tok) 
                                                             ,(token-len tok)))))))
      (init-token-atoms)))

;;; Lexer


(typedef uint32_t Position)

(struct Token
   (decl ((Atom*       atom)
          (const char* at))))

(struct Pos (decl ((int lin) (int col))))

; Comments (we leave these for later...)
; (enum (:COMMENT_MULTILINE :COMMENT_SINGLELINE))

(macrolet ((_at (x)     `(== (aref curr 0) ,x))
           (range (a b) `(&& (>= (aref curr 0) ,a) (>= (aref curr 0) ,b)))
           (digit ()    `(range #\0 #\9))
           (is-digit (x) `(&& (>= ,x #\0) (<= ,x #\9)))
           (at (x)
              (lisp 
                 (cond 
                    ((eq x :end)      `(_at 0))
                    ((eq x :space)    `(\|\| (_at #\Space) (_at #\Tab))) ; @Incomplete: Faltan '\v' i '\f'.
                    ((eq x :endl)     `(_at #\Newline))
                    ((eq x :id-start) `(\|\| (range #\a #\z) (range #\A #\Z) (_at #\_)))
                    ((eq x :id)       `(\|\| (range #\a #\z) (range #\A #\Z) (range #\0 #\9) (_at #\_)))
                    ((symbolp x)      `(_at ,x))
                    ((characterp x)   `(_at ,x))
                    ((integerp x)     `(_at ,x))
                    ((= 1 (length x)) `(_at ,(char x 0)))
                    (t `(&& ,@(loop for c across x
                                    for i from 0
                                  collect `(== (aref curr ,i) ,c)))))))
           (advance (n)  `(progn (+= curr ,n) (+= pos.col ,n)))
           (next-line () `(progn (postfix++ curr) (postfix++ pos.lin) (set pos.col 1)))
           (skip-comment (type)
              `(progn 
                  (advance 2)
                  (while 1
                     (when (at :end) (break))
                     ,(lisp (when (eq type :COMMENT_MULTILINE)
                               `(when (at "*/")
                                   (advance 2)
                                   (break))))
                     (cond ((at :endl) (next-line)
                                       ,(lisp (when (eq type :COMMENT_SINGLELINE)
                                                 `(break))))
                           (t (advance 1))))))
           (new-token (kind ptr size)
               `(cast Token (clist (funcall atom ,kind ,ptr ,size) ,ptr))))

   (struct lexer
      (decl ((const char* buffer)
             (const char* curr)
             (Pos         pos)))

      (function lexer ((const char* buf)) -> ()
         (set buffer buf))

      (function skip-space () -> bool
         ; Skip whitespace
         (decl ((const char* start = curr))
            (while 1
               (cond ((at :end)   (return (> curr start)))
                     ((at :space) (advance 1))
                     ((at :endl)  (next-line))
                     ((at "//")   (skip-comment :COMMENT_SINGLELINE))
                     ((at "/*")   (skip-comment :COMMENT_MULTILINE))
                     (t           (return (> curr start)))))))

      (function read-identifier () -> Token
         (decl ((const char* id-begin = curr))
            (advance 1)
            (while 1
               (cond ((at :end) (break))
                     ((at :id)  (advance 1))
                     (t (break))))
            (return (new-token :IDENT id-begin (- curr id-begin)))))

      (function read-number () -> Token
         (decl ((const char* id-begin = curr)
                (const char* id-end   = 0)
                (bool        real-number = false))
            (if (at #\-) (advance 1))
            (while 1
               (cond ((at :end) (break))
                     ((digit)   (advance 1))
                     ((at #\.)  (if real-number 
                                    (break)
                                    (progn (set real-number true)
                                       (advance 1))))))
            (set id-end curr)
            (decl ((Kind kind = :LIT-INT))
               (when real-number
                  (set kind :LIT-DOUBLE)
                  (when (at #\f)
                     (advance 1)
                     (set kind :LIT-FLOAT)))
               (return (new-token kind id-begin (- id-end id-begin))))))

      (function read-char-or-string-literal () -> Token
         (decl ((char delimiter = curr[0])
                (Kind kind      = (? (== delimiter #\') :LIT-CHAR :LIT-STRING)))
            (advance 1)
            (decl ((bool slash-error      = false)
                   (const char* tok-begin = curr)
                   (Pos         tokpos    = pos))
               (while 1
                  (cond ((at delimiter)  (break))
                        ((at :endl)      (return (new-token :ERROR tok-begin 0)))
                        ((at 92) ; #\\
                            (advance 1)
                            (switch curr[0]
                                ((#\a #\b #\f #\n #\r #\t #\v #\' #\" #\? 92) (break))
                                (t (set slash-error true)))))
                  (advance 1))
               (decl ((size_t len = (- curr tok-begin)))
                  (advance 1)
                  (if slash-error
                      (return (new-token :ERROR tok-begin 0))
                      (return (new-token kind tok-begin len)))))))

      (macrolet ((result (kind len)
                    `(block
                        (decl ((Token result = (clist ,(token-atom kind) curr)))
                           (advance ,len)
                           (return result))))
                 (at1 (ch) `(== (aref curr 1) ,ch))
                 (at2 (ch) `(== (aref curr 2) ,ch)))
         (function get () -> Token
            (if (at :END) (result :END 0))
            (skip-space)
            (switch curr[0]
               (#\( (result :LPAREN 1))
               (#\) (result :RPAREN 1))
               (#\[ (result :LBRACKET 1))
               (#\] (result :RBRACKET 1))
               (#\{ (result :LBRACE 1))
               (#\} (result :RBRACE 1))
               (#\; (result :SEMICOLON 1))
               (#\? (result :QMARK 1))
               (#\, (result :COMMA 1))
               (#\# (result :SHARP 1))
               
               (#\: (if (at1 #\:) (result :COLONCOLON 2) (result :COLON 1)))
               (#\+ (if (at1 #\+) (result :PLUSPLUS 2)   (result :PLUS 1)))
               (#\* (if (at1 #\=) (result :STAREQ 2)     (result :STAR 1)))
               (#\/ (if (at1 #\=) (result :DIVEQ 2)      (result :DIV 1)))
               (#\^ (if (at1 #\=) (result :XOREQ 2)      (result :XOR 1)))
               (#\& (if (at1 #\=) (result :AMPEQ 2)      (result :AMP 1)))
               (#\% (if (at1 #\=) (result :MODEQ 2)      (result :MOD 1)))
               (#\! (if (at1 #\=) (result :NOTEQ 2)      (result :NOT 1)))
               (#\= (if (at1 #\=) (result :EQEQ 2)       (result :EQ 1)))

               (#\. (if (is-digit (aref curr 1))
                        (return (read-number))
                        (result :DOT 1)))
               (#\| (switch curr[1]
                       (#\| (result :BARBAR 2))
                       (#\= (result :BAREQ 2))
                       (t   (result :BAR 1))))
               (#\< (switch curr[1]
                       (#\< (if (at2 #\=) (result :LSHIFTEQ 3) (result :LSHIFT 2)))
                       (#\= (result :LE 2))
                       (t   (result :LT 1))))
               (#\> (switch curr[1]
                       (#\> (if (at2 #\=) (result :RSHIFTEQ 3) (result :RSHIFT 2)))
                       (#\= (result :GE 2))
                       (t   (result :GT 1))))
               (#\- (switch curr[1]
                        (#\- (result :MINUSMINUS 2))
                        (#\= (result :MINUSEQ 2))
                        (#\> (result :ARROW 2))
                        (t   (if (is-digit curr[1]) 
                                 (return (read-number))
                                 (result :MINUS 1)))))

               ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (return (read-number)))
               ((#\' #\") (return (read-identifier)))
               ;(#\\ (result :BACKSLASH 1))
               (t   (if (at :id-start)
                        (return (read-identifier))
                        (result :ERROR 0)))
               )))))
#|
                  
|#

;; Main

(function main ((int argc) (char **argv)) -> int
   (init)
   (if (< argc 2) (die "usage: minicc <file>"))
   (decl ((char *filename = argv[1])
          (const char* buffer))
      (set buffer (read-whole-file filename))
      (printf "Buffer len = %lu\\n" (strlen buffer))))
