#include <iostream>
using namespace std;

#include "command.hh"

int Help(Args _) {
    constexpr const char *usage =
        "Usage: minicc [options...] [command]\n"
        "\n"
        "Simplified C++ compiler and interpreter for educational purposes.\n"
        "\n"
        "Commands:\n"
        "   config       Show config\n"
        "   lex          Lex a file\n"
        "   parse        Parse a file\n"
        "\n";

    cout << usage;
    return 1;
}
