
#include <iostream>
using namespace std;

void lee_segmento(int& n, char& c) {
   cin >> n >> c; // formato: "15x", "13*"
}

void escribe_segmento(int& n, char& c) {
   for (int i = 0; i < n; i++) {
      cout << c;
   }
}

int main() {
   int n;
   char c;
   lee_segmento(n, c);
   for (int i = 0; i < 5; i++) {
      escribe_segmento(n, c);
   }
}
[[out]]--------------------------------------------------

#include <iostream>
using namespace std;

void lee_segmento(int& n, char& c) {
   cin >> n >> c; // formato: "15x", "13*"
}

void escribe_segmento(int& n, char& c) {
   for (int i = 0; i < n; i++) {
      cout << c;
   }
}

int main() {
   int n;
   char c;
   lee_segmento(n, c);
   for (int i = 0; i < 5; i++) {
      escribe_segmento(n, c);
   }
}
