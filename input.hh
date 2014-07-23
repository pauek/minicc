#ifndef INPUT_H
#define INPUT_H

#include <iostream>
#include <vector>
#include "token.hh"

static const char* separators = ";,.[](){}+-*/=<>|&\n\t ";

struct Pos { 
   int lin, col; 
   Pos() : lin(1), col(0) {}
   std::string str() const;
};

struct Range {
   Pos ini, fin;
   Range(Pos i, Pos f) : ini(i), fin(f) {}
};

// Input /////////////////////////////////////////////////////////////

struct Comment;
struct CommentNode;

class Input {
   std::istream* _in;
   std::string _text;
   int _curr;
   Pos _pos;
   std::vector<int> _linepos; // positions of line starts (ignoring position 0 since no line 0)
   std::vector<std::pair<int, Pos>> _stack; // save/restore stack

   bool _seen_endl;

   void _reset() { 
      _pos.lin = 1;
      _pos.col = -1;
      _curr = -1; 
   }

   int _pos_to_idx(Pos p) const;

public:
                Input()                : _in(0), _linepos(1) { _reset(); }
                Input(std::istream* i) : _in(i), _linepos(1) { _reset(); }

          bool  next();
          bool  peek(int offset);
          Pos   pos()           const { return _pos; }
          char  curr(int i = 0) const { return _text[_curr + i]; }
          bool  end()           const { return _curr >= _text.size(); }
          bool  curr_one_of(std::string set) const;
   std::string  substr(const Pos& ini, const Pos& fin) const;
   std::string  substr(const Token& t);
          void  save();
          void  restore();
          void  discard();

          bool  seen_endl()     const { return _seen_endl; }
          void  mark()                { _seen_endl = false; }

          void  consume(char c);
          void  consume(std::string s);
   CommentNode *skip(std::string skip_set);
   std::string  skip_to(std::string stop_set);
   std::string  peek_to(std::string stop_set);
   std::string  skip_to_next_line();
   std::string  next_token_old() { return skip_to(separators); }
         Token  next_token();
         Token  peek_token();
          bool  expect(std::string word);
   
         Token  read_id();
         Token  read_operator();
         Token  read_number_literal();
         Token  read_string_or_char_literal(char delim);
          void  read_singleline_comment(Comment& c);
          void  read_multiline_comment(Comment& c);

          void  error(std::string msg);
};

bool is_space(std::string s);

inline std::ostream& operator<<(std::ostream& o, const Pos& pos) {
   o << pos.lin << ":" << pos.col;
   return o;
}

inline bool operator==(const Pos& a, const Pos& b) {
   return a.lin == b.lin && a.col == b.col;
}

inline bool operator<(const Pos& a, const Pos& b) {
   if (a.lin != b.lin) {
      return a.lin < b.lin;
   } else {
      return a.col < b.col;
   }
}

inline bool operator>(const Pos& a, const Pos& b) {
   if (a.lin != b.lin) {
      return a.lin > b.lin;
   } else {
      return a.col > b.col;
   }
}

inline bool operator<=(const Pos& a, const Pos& b) { 
   return operator<(a, b) || operator==(a, b); 
}

inline bool operator>=(const Pos& a, const Pos& b) { 
   return operator>(a, b) || operator==(a, b); 
}

#endif
