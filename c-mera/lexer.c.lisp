
(include <stdio.h>)
(include <stdlib.h>)
(include <stdint.h>)

(defmacro open-file (filename file)
   `(progn
      (set ,file (fopen ,filename "r"))
      (if (== ,file 0) (return 0))))

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
         (when (!= n-bytes-read (cast size_t ,size))
            (err "Couldn't read the whole file")
            (free ,buffer)
            (return 0)))))

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

(enum TokenKind (:TOK_EOF :TOK_ERROR :TOK_BACKSLASH :TOK_DIRECTIVE :TOK_FILENAME))

(struct Token
   (decl ((TokenKind kind)
          (uint32_t  pos)
          (char*     atom))))

(function main ((int argc) (char **argv)) -> int
   (decl ((char* data))
      (read-whole-file "lexer.c.lisp")))
