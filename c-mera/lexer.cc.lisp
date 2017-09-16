
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
; 2. a list   = (:eof "<eof>")  ; first item is a symbol, second is the associated string
;                               ; the third item is optional and is the size of the token
;                               ; this is normally computed from the string but for "\\\\" should be 1...

(defvar control
   '((:eof "<eof>") (:error "<error>") (:backlash "\\\\" 1) (:sharp "#")))
(defvar punctuation
   '((:colon     ";") (:semicolon ";") (:coloncolon "::") (:comma ",") (:qmark "?")))
(defvar logical
   '((:barbar "||") (:ampamp "&&") (:or "or") (:and "and")))
(defvar comparison
   '((:eqeq "==") (:noteq "!=") (:lt "<") (:gt ">") (:le "<=") (:ge ">=")))
(defvar access
   '((:dot ".") (:arrow "->") (:arrowp "->*")))
(defvar bitwise
   '((:not "!") (:amp "&") (:bar "|") (:xor "^") (:lshift "<<") (:rshift ">>")))
(defvar arithmetic
   '((:plus "+") (:minus "-") (:star "*") (:mod "%")))
(defvar increment
   '((:plusplus "++") (:minusminus "--")))
(defvar assign
   '((:eq "=") (:stareq "*=") (:minuseq "-=") (:pluseq "+=") (:slasheq "/=")
     (:modeq "%=") (:bareq "|=") (:ampeq "&=") (:xoreq "^=") 
     (:lshifteq "<<=") (:rshifteq ">>=")))
(defvar delimiters
   '((:lbrace "{") (:rbrace "}") (:lparen "(") (:rparen ")") 
     (:lbracket "[") (:rbracket "]")))
(defvar keywords
   '(:if :else :while :for :switch :case :break :continue :goto :return
     :using :namespace :struct :class :typedef :enum
     :void :int :bool :char :float :double :string
     :short :long :const :unsigned
     :auto :register :static :extern :volatile :mutable
     :inline :virtual :explicit
     :true :false))
(defvar macros
   '(:include))

(lisp ; Porqué hay que poner este 'lisp' aquí??
   (defvar tokens
      (loop for tok in (append control punctuation logical comparison access bitwise 
                               arithmetic increment assign delimiters keywords macros)
         collect  (if (symbolp tok)
                     (let ((str (string-downcase (symbol-name tok))))
                        `(,tok ,str ,(length str)))
                     (destructuring-bind (sym str &optional (size 'nil)) tok
                        (if (null size) (setf size (length str)))
                           `(,sym ,str ,size))))))

(defun tok-sym  (tok) (nth 0 tok))
(defun tok-str  (tok) (nth 1 tok))
(defun tok-len  (tok) (nth 2 tok))
(defun tok-atom (tok) (make-symbol (format nil "_atom_~a" (tok-sym tok))))

;;; Atoms

(struct Atom
   (decl ((size_t      len)
          (const char* str))))

(struct Node
   (decl ((Atom  atom)
          (Node* prev))))

(decl ((Node* (array nodes ATOM_NUM_NODES))))

(macrolet ((define-token-atoms ()
              `(decl (,@(loop for tok in tokens collect 
                  `(Atom* ,(tok-atom tok)))))))
   
   (define-token-atoms))

(function hash ((const char* p) (size_t len)) -> uint32_t
   (decl ((uint32_t r = 2166136261))
      (for ((uint32_t i = 0) (< i len) i++)
         (set r (^ r *p++))
         (set r (* r 16777619)))
      (return r)))

(defmacro _new (type)
   `(cast (postfix* ,type) (malloc (sizeof ,type))))

(function atom ((const char *str) (size_t len)) -> Atom*
   (decl ((uint32_t mask = (1- ATOM_NUM_NODES))
          (uint32_t idx  = (& (hash str len) mask))
          (Node*    n))
      (set n (aref nodes idx))
      (while n
         (if (and (== n->atom.len len) (== 0 (strncmp n->atom.str str len)))
            (return (& n->atom)))
         (set n n->prev))
      (set n (_new Node));(cast Node* (malloc (sizeof Node))))
      (set n->atom.str str)
      (set n->atom.len len)
      (set n->prev (aref nodes idx))
      (set (aref nodes idx) n)
      (return (& n->atom))))

(function atom ((const char* str)) -> Atom*
   (return (funcall atom str (strlen str))))

(function init () -> void
   (macrolet ((init-token-atoms ()
                  `(progn ,@(loop for tok in tokens collect 
                               `(set ,(tok-atom tok) 
                                     (funcall atom ,(tok-str tok) ,(tok-len tok)))))))
      (init-token-atoms)))

;;; Lexer

(enum TokenKind 
   (:TOK_EOF :TOK_ERROR :TOK_BACKSLASH :TOK_DIRECTIVE :TOK_FILENAME
    :TOK_USING :TOK_PUNCT :TOK_DELIM :TOK_OPERATOR :TOK_IDENT :TOK_CONTROL
    :TOK_TYPEDEF :TOK_MODIFIER :TOK_TYPE 
    :TOK_LIT_BOOL :TOK_LIT_INT :TOK_LIT_FLOAT :TOK_LIT_DOUBLE :TOK_LIT_CHAR 
    :TOK_LIT_STRING))

(typedef uint32_t Position)

(struct Token
   (decl ((TokenKind   kind)
          (Atom*       atom)
          (const char* at))))

(struct Pos (decl ((int lin) (int col))))

; Comments (we leave these for later...)
; (enum (:COMMENT_MULTILINE :COMMENT_SINGLELINE))

(macrolet ((_at (x)     `(== (aref curr 0) ,x))
           (range (a b) `(&& (>= (aref curr 0) ,a) (>= (aref curr 0) ,b)))
           (digit ()    `(range #\0 #\9))
           (at (x)
              (lisp 
                 (cond 
                    ((eq x :eof)      `(_at 0))
                    ((eq x :space)    `(\|\| (_at #\Space) (_at #\Tab))) ; @Incomplete: Faltan '\v' i '\f'.
                    ((eq x :endl)     `(_at #\Newline))
                    ((eq x :id)       `(\|\| (range #\a #\z) (range #\A #\Z) (range #\0 #\9) (_at #\_)))
                    ((symbolp x)      `(_at ,x))
                    ((characterp x)   `(_at ,x))
                    ((integerp x)     `(_at ,x))
                    ((= 1 (length x)) `(_at ,(char x 0)))
                    (t `(&& ,@(loop for c across x
                                    for i from 0
                                  collect `(== (aref curr ,i) ,c)))))))
           (advance (n) `(progn (+= curr ,n) (+= pos.col ,n)))
           (skip-comment (type)
              `(progn 
                  (advance 2)
                  (while 1
                     (when (at :eof) break)
                     (when (at :endl)
                        (postfix++ curr) 
                        (postfix++ pos.lin) 
                        (set pos.col 1)
                        ,(lisp (when (eq type :COMMENT_SINGLELINE)
                                 `(break))))
                     ,(lisp (when (eq type :COMMENT_MULTILINE)
                               `(when (at "*/")
                                   (advance 2)
                                   (break))))
                     (advance 1)))))

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
               (cond ((at :eof)   (return (> curr start)))
                     ((at :space) (advance 1))
                     ((at :endl)  (postfix++ curr) (postfix++ pos.lin) (set pos.col 1))
                     ((at "//")   (skip-comment :COMMENT_SINGLELINE))
                     ((at "/*")   (skip-comment :COMMENT_MULTILINE))
                     (t           (return (> curr start)))))))

      (function read-identifier () -> Token
         (decl ((const char* id-begin = curr))
            (advance 1)
            (while 1
               (cond ((at :eof) (break))
                     ((at :id)  (advance 1))
                     (t (break))))
            (return (cast Token (clist TOK_IDENT 
                                       (funcall atom id-begin (cast size_t (- curr id-begin)))
                                       id-begin)))))

      (function read-number () -> Token
         (decl ((const char* id-begin = curr)
                (const char* id-end   = 0)
                (bool        real-number = false))
            (if (at #\-) (advance 1))
            (while 1
               (cond ((at :eof) (break))
                     ((digit)   (advance 1))
                     ((at #\.)  (if real-number 
                                    (break)
                                    (progn (set real-number true)
                                       (advance 1))))))
            (set id-end curr)
            (decl ((TokenKind kind = TOK_LIT_INT))
               (when real-number
                  (set kind TOK_LIT_DOUBLE)
                  (when (at #\f)
                     (advance 1)
                     (set kind TOK_LIT_FLOAT)))
               (return (cast Token (clist kind
                                          (funcall atom id-begin (- id-end id-begin)) 
                                          id-begin))))))

      (function read-char-or-string () -> Token
         (decl ((char delimiter = curr[0])
                (TokenKind kind = (? (== delimiter #\') TOK_LIT_CHAR TOK_LIT_STRING)))
            (advance 1)
            (decl ((bool slash-error      = false)
                   (int  nchars           = 0)
                   (const char* tok-begin = curr)
                   (Pos         tokpos    = pos))
               (while 1
                  (cond ((at delimiter)  (break))
                        ((at :endl)      (return (cast Token (clist TOK_ERROR 0 tok-begin))))
                        ((at 92) ; #\\
                            (advance 1)
                            (switch curr[0]
                                ((#\a #\b #\f #\n #\r #\t #\v #\' #\" #\? 92) (break))
                                (t (set slash-error true)))))
                  (advance 1)
                  nchars++)
               (decl ((size_t len = (- curr tok-begin)))
                  (advance 1)
                  (if slash-error
                      (return (cast Token (clist TOK_ERROR 0 tok-begin)))
                      (return (cast Token (clist kind (funcall atom tok-begin len) tok-begin))))))))

      (function get () -> Token
         (when (at :eof) 
            (return (cast Token (clist TOK_EOF 0 curr))))
         (skip-space)
         ;;; GENERATE HUGE SWITCH FROM TOKENS!!!!
         ; 1. Add all words to hash table or equivalent
         ; 2. Generate cases from each key-value pair.
         )))

;; Main

(function main ((int argc) (char **argv)) -> int
   (init)
   (if (< argc 2) (die "usage: minicc <file>"))
   (decl ((char *filename = argv[1])
          (const char* buffer))
      (set buffer (read-whole-file filename))
      (printf "Buffer len = %lu\\n" (strlen buffer))))
