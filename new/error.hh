#ifndef ERROR_HH
#define ERROR_HH

#include <iostream>

#include "tokens.hh"

class Error {
    std::string filename;
    LinCol      pos;
    std::string msg;

   public:
    Error(std::string filename_, LinCol pos_, std::string msg_)
        : filename(filename_), pos(pos_), msg(msg_) {}

    void print(std::ostream& out) {
        out << filename << ":" << pos << ": " << error_type() << " error: " << msg << std::endl;
    }

    virtual const char *error_type() = 0;
};

class LexError : public Error {
   public:
    LexError(std::string filename_, LinCol pos_, std::string msg_) : Error(filename_, pos_, msg_) {}

    const char* error_type() override { return "lex"; }
};

class ParseError : public Error {
   public:
    ParseError(std::string filename_, LinCol pos_, std::string msg_)
        : Error(filename_, pos_, msg_) {}

    const char* error_type() override { return "parse"; }
};

#endif
