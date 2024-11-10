#ifndef ERROR_HH
#define ERROR_HH

#include <string>
#include "pos.hh"

struct ErrorOptions {
    bool stopper = false;
};

struct Error {
    Span        span;
    std::string msg;
    bool        stopper;  // this error should eclipse the following errors
                          // (probably an avalanche of parsing errors)

    Error(std::string m) : stopper(false), msg(m) {}

    Error(Pos p, std::string m) : stopper(false), span(p), msg(m) {}

    Error(Span s, std::string m, ErrorOptions options = {.stopper = false})
        : stopper(options.stopper), span(s), msg(m) {}

    void to_json(std::ostream& o) const;
};

struct EvalError : public Error {
    EvalError(std::string _msg) : Error(_msg) {}
};

struct ParseError {
    Pos         pos;
    std::string msg;

    ParseError() : pos(-1, 0) {}

    ParseError(Pos p, std::string m) : pos(p), msg(m) {}

    bool empty() const { return pos.lin == -1; }
};

#endif