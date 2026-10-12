#ifndef ERROR_HH
#define ERROR_HH

#include <iostream>

#include "tokens.hh"

class Error {
   protected:
    std::string filename;
    LinCol      pos;
    std::string msg;

   public:
    Error(std::string filename_, LinCol pos_, std::string msg_)
        : filename(filename_), pos(pos_), msg(msg_) {}

    virtual void print(std::ostream& out) {
        out << filename << ":" << pos << ": " << error_type() << " error: ";
        out << msg << std::endl;
    }

    virtual const char *error_type() = 0;
};

class LexError : public Error {
   public:
    LexError(std::string filename, LinCol pos_, std::string msg_) : Error(filename, pos_, msg_) {}

    const char *error_type() override { return "lex"; }
};

class ParseError : public Error {
    std::string codeline;

   public:
    ParseError(const Tokens& tokens, LinCol pos_, std::string msg_)
        : Error(tokens.filename_, pos_, msg_) {
        codeline = tokens.get_line(pos.lin);
    }

    const char *error_type() override { return "parse"; }

    void print(std::ostream& out) override {
        out << filename << ":" << pos << ": " << "parse error: " << msg << std::endl;
        out << "    | " << codeline;
        out << "    | " << std::string(pos.col - 1, ' ') << '^' << std::endl;
    }
};

#endif
