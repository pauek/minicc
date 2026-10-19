#ifndef UTILS_HH
#define UTILS_HH

#include <format>
#include <iostream>
#include <string>

std::string read_file(std::string filename);
std::string read_stdin();
std::string trim(std::string s, size_t n = 1);

template <class... Args>
void error(std::format_string<Args...> fmt, Args&&...args) {
    std::cerr << format(fmt, args...) << std::endl;
    exit(1);
}

bool all_spaces(std::string line);
bool allowed_char(char c);

struct NameLevel {
    std::string name;
    uint8_t     level;
};
NameLevel split_config_line(std::string filename, std::string line, int lin);

#endif
