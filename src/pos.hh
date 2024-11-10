#ifndef POS_H
#define POS_H
#include <cstdint>
#include <sstream>

struct Pos {
    uint32_t lin, col;

    Pos() : lin(1), col(0) {}

    explicit Pos(uint32_t l, uint32_t c) : lin(l), col(c) {}

    std::string str() const;
    void        to_json(std::ostream& o) const;
};

struct Span {
    Pos begin, end;

    Span() {}

    explicit Span(Pos p) : begin(p), end(p) {}

    explicit Span(Pos b, Pos e) : begin(b), end(e) {}
};

inline std::ostream& operator<<(std::ostream& o, const Pos& pos) {
    return o << pos.lin << ":" << pos.col;
}

inline std::ostream& operator<<(std::ostream& o, const Span& rng) {
    return o << rng.begin << "-" << rng.end;
}

inline std::string Pos::str() const {
    std::ostringstream out;
    out << lin << ':' << col;
    return out.str();
}

inline void Pos::to_json(std::ostream& o) const {
    o << "{\"lin\": " << lin << ", \"col\": " << col << "}";
}
#endif
