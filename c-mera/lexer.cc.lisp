
(include <stdio.h>)
(include <stdlib.h>)
(include <stdint.h>)
(include <string.h>)

;;; Configuration

(lisp 
   (defvar ATOM_NUM_NODES 4096)) ; Tiene que ser potencia de 2

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

(lisp (defvar tokens '(
   (:eof        "<eof>"     5)
   (:error      "<error>"   7)
   (:backslash  "\\\\"      1)

   (:semicolon  ";"         1)
   (:colon      ":"         1)
   (:coloncolon "::"        2)
   (:comma      ","         1)
   (:qmark      "?"         1)

   (:barbar     "||"        2)
   (:ampamp     "&&"        2)
   (:or         "or"        2)
   (:and        "and"       3)

   (:eqeq       "=="        2)
   (:noteq      "!="        2)
   (:lt         "<"         1)
   (:gt         ">"         1)
   (:leq        "<="        2)
   (:geq        ">="        2)

   (:dot        "."         1)
   (:arrow      "->"        2)
   (:arrowp     "->*"       3)

   (:not        "!"         1)
   (:amp        "&"         1)
   (:bar        "|"         1)
   (:xor        "^"         1)
   (:plus       "+"         1)
   (:minus      "-"         1)
   (:star       "*"         1)
   (:slash      "/"         1)
   (:mod        "%"         1)
   (:lshift     "<<"        2)
   (:rshift     ">>"        2)
   (:plusplus   "++"        2)
   (:minusminus "--"        2)

   (:eq         "="         1)
   (:stareq     "*="        2)
   (:minuseq    "-="        2)
   (:pluseq     "+="        2)
   (:slasheq    "/="        2)
   (:modeq      "%="        2)
   (:bareq      "|="        2)
   (:ampeq      "&="        2)
   (:xoreq      "^="        2)
   (:lshifteq   "<<="       3)
   (:rshifteq   ">>="       3)

   (:sharp      "#"         1)
   (:lbrace     "{"         1)
   (:rbrace     "}"         1)
   (:lparen     "("         1)
   (:rparen     ")"         1)
   (:lbracket   "["         1)
   (:rbracket   "]"         1)

   (:if         "if"        2)
   (:else       "else"      4)
   (:while      "while"     5)
   (:for        "for"       3)
   (:switch     "switch"    6)
   (:case       "case"      4)
   (:break      "break"     5)
   (:continue   "continue"  8)
   (:goto       "goto"      4)
   (:return     "return"    6)

   (:using      "using"     5)
   (:namespace  "namespace" 9)
   (:struct     "struct"    6)
   (:class      "class"     4)
   (:typedef    "typedef"   7)
   (:enum       "enum"      4)

   (:void       "void"      4)
   (:int        "int"       3)
   (:bool       "bool"      4)
   (:char       "char"      4)
   (:float      "float"     5)
   (:double     "double"    6)
   (:string     "string"    6)

   (:short      "short"     5)
   (:long       "long"      4)
   (:const      "const"     5)
   (:unsigned   "unsigned"  8)

   (:auto       "auto"      4)
   (:register   "register"  8)
   (:static     "static"    6)
   (:extern     "extern"    6)
   (:volatile   "volatile"  7)
   (:mutable    "mutable"   7)

   (:inline     "inline"    6)
   (:virtual    "virtual"   7)
   (:explicit   "explicit"  8)

   (:true       "true"      4)
   (:false      "false"     5)

   (:include    "include"   7)
)))

(defun tok-sym  (tok) (nth 0 tok))
(defun tok-str  (tok) (nth 1 tok))
(defun tok-len  (tok) (nth 2 tok))
(defun tok-atom (tok) (make-symbol (format nil "_~a_" (tok-sym tok))))

;;; Atom

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
    :TOK_LIT_BOOL :TOK_LIT_INT :TOK_LIT_FLOAT :TOK_LIT_DOUBLE :TOK_LIT_CHAR :TOK_LIT_STRING))

(typedef uint32_t Position)

(struct Token
   (decl ((TokenKind kind)
          (char*     str)
          (size_t    size))))

(enum (:COMMENT_MULTILINE :COMMENT_SINGLELINE))


(function main ((int argc) (char **argv)) -> int
   (init)
   (if (< argc 2) (die "usage: minicc <file>"))
   (decl ((char *filename = argv[1])
          (const char* buffer))
      (set buffer (read-whole-file filename))
      (printf "Buffer len = %lu\\n" (strlen buffer))))
