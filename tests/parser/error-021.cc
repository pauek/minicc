#include <iostream>
using namespace std;

// Programa que donat un numero n 
// calcula els passos per dur a terme la sequencia de Collatz

int main() {
  int i = 0;
  int n;
   while (cin >> n) {
      for (int n; n > 1; i = i + 1) {
      if (n % 2 == 0) {
	  n .......= n / 2;
      }
      if (n % 2 != 0) {
	  n = n * 3 + 1;
      }
    }
  cout << i << endl;
  }
}
[[err]]-----------------
tests/parser/error-021.cc[13:7]: Expected identifier.