#ifndef INPUT_H
#define INPUT_H

#include <iostream>
#include <vector>
#include "token.hh"

struct Pos { 
   int lin, col; 
   Pos() : lin(1), col(0) {}
   Pos(int l, int c) : lin(l), col(c) {}
   std::string str() const;
   
    int l() const { return lin; }
    int c() const { return col; }
   void sl(int x) { lin = x; }
   void sc(int x) { col = x; }
};

struct Range {
   Pos ini, fin;
   Range() {}
   Range(Pos i, Pos f) : ini(i), fin(f) {}

    Pos gi() const { return ini; }
    Pos gf() const { return fin; }
   void si(const Pos& p) { ini = p; }
   void sf(const Pos& p) { fin = p; }
};

// Input /////////////////////////////////////////////////////////////

struct Comment;
struct CommentSeq;

class Input {
   std::istream* _in;
   std::string _text;
   int _curr;
   Pos _pos;
   std::vector<int> _linepos; // positions of line starts (ignoring position 0 since no line 0)

   struct SavedItem {
      int              curr;
      Pos              pos;
      std::vector<int> linepos;

      SavedItem(int c, Pos p, const std::vector<int>& l)
         : curr(c), pos(p), linepos(l) {}
   };
   std::vector<SavedItem> _stack; // save/restore stack

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
   std::string  substr(const Range& r) const { return substr(r.ini, r.fin); }
   std::string  substr(const Pos& ini, const Pos& fin) const;
   std::string  substr(const Token& t);

          void  save();
          void  restore();
          void  discard();

          void  mark()                { _seen_endl = false; }

          bool  expect(std::string word);
          void  consume(char c);
          void  consume(std::string s);
   CommentSeq *skip(std::string skip_set);
   std::string  skip_to(std::string stop_set);

         Token  next_token();
         Token  peek_token();
         Token  read_id();
         Token  read_operator();
         Token  read_number_literal();
         Token  read_real_literal(Token t);
         Token  read_string_or_char_literal(char delim);
          void  read_singleline_comment(Comment& c);
          void  read_multiline_comment(Comment& c);

          void  error(std::string msg);
};

bool is_space(std::string s);

inline std::ostream& operator<<(std::ostream& o, const Pos& pos) {
   return o << pos.lin << ":" << pos.col;
}

inline std::ostream& operator<<(std::ostream& o, const Range& rng) {
   return o << rng.ini << "-" << rng.fin;
}

inline bool operator==(const Pos& a, const Pos& b) {
   return a.lin == b.lin && a.col == b.col;
}

inline bool operator<(const Pos& a, const Pos& b) {
   return (a.lin != b.lin ? a.lin < b.lin : a.col < b.col);
}

inline bool operator>(const Pos& a, const Pos& b) {
   return (a.lin != b.lin ? a.lin > b.lin : a.col > b.col);
}

inline bool operator<=(const Pos& a, const Pos& b) { 
   return operator<(a, b) || operator==(a, b); 
}

inline bool operator>=(const Pos& a, const Pos& b) { 
   return operator>(a, b) || operator==(a, b); 
}

inline Pos operator+(const Pos& p, int n) {
   return Pos(p.lin, p.col + n);
}

inline Pos operator-(const Pos& p, int n) {
   return Pos(p.lin, p.col - n);
}

#endif
