#ifndef UTILS_HH
#define UTILS_HH

#include <string>

std::string read_file(std::string filename);
std::string read_stdin();
std::string trim(std::string s, size_t n = 1);

#endif
