#ifndef POS_H
#define POS_H

struct Pos { 
   int lin, col; 
   Pos() : lin(1), col(0) {}
   Pos(int l, int c) : lin(l), col(c) {}
   std::string str() const;
   
    int l() const { return lin; }
    int c() const { return col; }
   void sl(int x) { lin = x; }
   void sc(int x) { col = x; }

    Pos next() const { return Pos(lin, col+1); }

   void to_json(std::ostream& o) const;
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
