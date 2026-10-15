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

        out << "    | ";

        // NOTE(pauek): When printing the circumflex to show the error, we have the problema that
        // the codeline might have spaces or tabs, so we can't just offset the arrow by printing
        // spaces before it... ;)
        //
        // We take the codeline and print a version keeping the spaces but replacing
        // non-spaces with a single space. This will print exactly the same way with
        // regard to tabs, and will position the arrow correctly.
        //
        for (size_t i = 0; i < pos.col - 1; i++) {
        	char c = codeline[i];
         	out << (isspace(c) ? c : ' ');
        }
        out << '^' << std::endl;
    }
};

#endif
