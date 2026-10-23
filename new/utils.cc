#include <filesystem>
#include <fstream>
#include <iostream>
#include <string>
using namespace std;

#include "utils.hh"

string read_file(string filename) {
    auto     size = filesystem::file_size(filename);
    string   content(size, '\0');
    ifstream file(filename);
    if (file.bad()) {
        cerr << "Bad!" << endl;
    }
    file.read(&content[0], size);
    return content;
}

string read_stdin() {
    string input, line;
    while (getline(cin, line)) {
        // FIXME: This is VERY inefficient!!
        input += line + '\n';
    }
    return input;
}

string trim(string s, size_t n) {
    size_t size = s.size();
    if (2 * n >= size) {
        return "";
    }
    return s.substr(n, size - 2 * n);
}

bool allowed_char(char c) {
    return isalpha(c) || c == '_';
}

bool all_spaces(std::string line) {
    for (char c : line) {
        if (not isspace(c)) {
            return false;
        }
    }
    return true;
}
