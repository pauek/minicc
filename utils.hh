#ifndef UTILS_HH
#define UTILS_HH

#include <string>

#include "tokens.hh"

std::string read_file(std::string filename);
std::string read_stdin();
std::string trim(std::string s, size_t n = 1);

bool all_spaces(std::string line);
bool allowed_char(char c);

void print_error(
    std::ostream& out,
    std::string   filename,
    LinCol        pos,
    std::string   msg,
    std::string   codeline
);

#endif
