
;;; Configuration

(defvar ATOM_NUM_NODES 4096) ; Tiene que ser potencia de 2

;;; Token list

(defstruct token kind str len)
(defvar token-table (make-hash-table))

(defun token-atom   (sym) (make-symbol (string-upcase (format nil "_ATOM_~a" sym))))
(defun token-length (sym) (token-len (gethash sym token-table)))

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

(deftoken :END   :end   "\\0" 1)
(deftoken :ERROR :error "<error>")
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

(defkeyword-list
   :if :else :while :for :switch :case :break :continue :goto :return
   :using :namespace                                  ; :USING
   :typedef :enum :struct :class                      ; :TYPEDEF
   :void :int :bool :char :float :double :string      ; :TYPE
   :short :long :const :unsigned                      ; :MODIFIER
   :auto :register :static :extern :volatile :mutable ; :MODIFIER
   :inline :virtual :explicit                         ; :MODIFIER
   :true :false)                                      ; :LIT-BOOL

;;; Atoms

(defvar kinds 
   '(:END :ERROR :BACKSLASH :DIRECTIVE :FILENAME
     :USING :PUNCT :DELIM :OPERATOR :ASSIGN :KEYWORD
     :IDENT :CONTROL :MODIFIER :TYPE 
     :LIT-BOOL :LIT-INT :LIT-FLOAT :LIT-DOUBLE 
     :LIT-CHAR :LIT-STRING))

(macrolet ((enum-kinds () `(enum TokenKind ,kinds)))
   (enum-kinds))

(macrolet ((fun-kind2str () 
              `(function kind2str ((uint32_t kind)) -> (const char*)
                   (switch kind ,@(loop for k in kinds collect `(,k (return ,(string-upcase (symbol-name k))))))
                   (return "<unknown>"))))
   (fun-kind2str))

(struct Atom
   (decl ((uint32_t    kind)
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

(function atom ((uint32_t kind) (const char *str) (size_t len)) -> Atom*
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

(function atom ((uint32_t kind) (const char* str)) -> Atom*
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
   (decl ((Atom*    atom)
          (uint32_t pos)))) ; La posición es un offset con respecto al inicio del buffer

(struct Pos (decl ((int lin) (int col))))

; Comments (we leave these for later...)
; (enum (:COMMENT_MULTILINE :COMMENT_SINGLELINE))

(macrolet ((_at (x)     `(== (aref curr 0) ,x))
           (range (a b) `(&& (>= (aref curr 0) ,a) (<= (aref curr 0) ,b)))
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
           (advance (n)  `(+= curr ,n))
           (next-line () `(progn (postfix++ curr)))
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
               `(cast Token (clist (funcall atom ,kind ,ptr ,size) (- ,ptr buffer)))))

   (struct Lexer
      (decl ((const char* buffer)
             (const char* curr)))

      (function init ((const char* buf)) -> void
         (set buffer buf)
         (set curr   buf))

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
         (decl ((const char* start = curr))
            (advance 1)
            (while 1
               (cond ((at :end) (break))
                     ((at :id)  (advance 1))
                     (t (break))))
            (return (new-token :IDENT start (- curr start)))))

      (function read-number () -> Token
         (decl ((const char* start       = curr)
                (bool        real-number = false)
                (uint32_t    kind))
            (if (at #\-) (advance 1))
            (while 1
               (cond ((at :end) (break))
                     ((digit)   (advance 1))
                     ((at #\.)  (if real-number (break))
                                (set real-number true)
                                (advance 1))
                     (t (break))))
            (set kind :LIT-INT)
            (when real-number
               (set kind :LIT-DOUBLE)
               (when (at #\f)
                  (advance 1)
                  (set kind :LIT-FLOAT)))
            (return (new-token kind start (- curr start)))))

      (function read-char-or-string-literal () -> Token
         (decl ((char      delimiter = curr[0])
                (uint32_t  kind      = (? (== delimiter #\') :LIT-CHAR :LIT-STRING)))
            (advance 1)
            (decl ((bool slash-error   = false)
                   (const char* start  = curr))
               (while 1
                  (cond ((at delimiter)  (break))
                        ((at :endl)      (return (new-token :ERROR start 0)))
                        ((at 92) ; #\\
                            (advance 1)
                            (switch curr[0]
                                ((#\a #\b #\f #\n #\r #\t #\v #\' #\" #\? 92) (break))
                                (t (set slash-error true)))))
                  (advance 1))
               (decl ((size_t len = (- curr start)))
                  (advance 1) ; consume delimiter
                  (if slash-error
                      (return (new-token :ERROR start 0))
                      (return (new-token kind start len)))))))

      (macrolet ((result (sym)
                    `(block
                        (decl ((Token result = (clist ,(token-atom sym) (- curr buffer))))
                           (advance ,(token-length sym))
                           (return result))))
                 (at1 (ch) `(== (aref curr 1) ,ch))
                 (at2 (ch) `(== (aref curr 2) ,ch))
                 (if1 (ch a b) `(if (at1 ,ch) (result ,a) (result ,b))))
         (function get () -> Token
            (if (at :END) (result :end))
            (skip-space)
            (switch curr[0]
               (#\( (result :lparen))
               (#\) (result :rparen))
               (#\[ (result :lbracket))
               (#\] (result :rbracket))
               (#\{ (result :lbrace))
               (#\} (result :rbrace))
               (#\; (result :semicolon))
               (#\? (result :qmark))
               (#\, (result :comma))
               (#\# (result :sharp))
               
               (#\: (if1 #\: :coloncolon :colon))
               (#\+ (if1 #\+ :plusplus   :plus))
               (#\* (if1 #\= :stareq     :star))
               (#\/ (if1 #\= :diveq      :div))
               (#\^ (if1 #\= :xoreq      :xor))
               (#\& (if1 #\= :ampeq      :amp))
               (#\% (if1 #\= :modeq      :mod))
               (#\! (if1 #\= :noteq      :not))
               (#\= (if1 #\= :eqeq       :eq))

               (#\. (if (is-digit (aref curr 1))
                        (return (read-number))
                        (result :dot)))
               (#\| (switch curr[1]
                       (#\| (result :barbar))
                       (#\= (result :bareq))
                       (t   (result :bar))))
               (#\< (switch curr[1]
                       (#\< (if (at2 #\=) (result :lshifteq) (result :lshift)))
                       (#\= (result :le))
                       (t   (result :lt))))
               (#\> (switch curr[1]
                       (#\> (if (at2 #\=) (result :rshifteq) (result :rshift)))
                       (#\= (result :ge))
                       (t   (result :gt))))
               (#\- (switch curr[1]
                        (#\- (result :minusminus))
                        (#\= (result :minuseq))
                        (#\> (result :arrow))
                        (t   (if (is-digit curr[1]) 
                                 (return (read-number))
                                 (result :minus)))))

               ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (return (read-number)))
               ((#\' #\") (return (read-char-or-string-literal)))
               ;(#\\ (result :BACKSLASH 1))
               (t   (if (at :id-start)
                        (return (read-identifier))
                        (result :error))))))))
