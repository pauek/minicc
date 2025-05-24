#include <iostream>
using namespace std;

/**
 * @pre  n >= 0
 * @post retorna cert si n és bicolor, fals en cas contrari
 */
bool is_bicolor(int n) {
    string s = to_string(n);
    int len = s.size();

    // Necessitem almenys 2 dígits per formar dos blocs
    if (len < 2) return false;

    // Prova totes les possibles divisions de s en dos blocs consecutius
    for (int k = 1; k < len; ++k) {
        string first = s.substr(0, k);
        string second = s.substr(k);

        char d = first[0];
        char e = second[0];

        // Els dos dígits han de ser diferents
        if (d == e) continue;

        // Verifica si tots els caràcters de first són iguals a d
        bool valid_first = all_of(first.begin(), first.end(), [d](char c) { return c == d; });

        // Verifica si tots els caràcters de second són iguals a e
        bool valid_second = all_of(second.begin(), second.end(), [e](char c) { return c == e; });

        if (valid_first && valid_second) return true;
    }

    return false;
}

int main() {
    int n;
    while (cin >> n) {
        cout << is_bicolor(n) << endl;
    }
}
[[err]]------------------------
tests/parser/error-017.cc[27:13]: Esperaba un ';'.