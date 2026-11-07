#include <filesystem>
#include <fstream>
#include <iostream>
#include <string>
using namespace std;

#include "tokens.hh"
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

void print_error(ostream& out, string filename, LinCol pos, string msg, string codeline) {
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
