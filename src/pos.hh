#ifndef POS_H
#define POS_H

struct Pos { 
   uint32_t lin, col; 

   Pos() : lin(1), col(0) {}
   Pos(uint32_t l, uint32_t c) : lin(l), col(c) {}
   std::string str() const;
   
   void to_json(std::ostream& o) const;
};

struct Range {
   Pos ini, fin;
   Range() {}
   Range(Pos i, Pos f) : ini(i), fin(f) {}
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

// FIXME: Esto es una burrada en realidad, cada línea tiene un tamaño distinto y no sabes si te pasas o qué...
inline Pos operator+(const Pos& p, int n) {
   return Pos(p.lin, p.col + n);
}

inline Pos operator-(const Pos& p, int n) {
   return Pos(p.lin, p.col - n);
}

#endif
