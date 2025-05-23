#include <iostream>
using namespace std;

int main() {
   string line;
   getline(cin, line);
   int pos = 0;
   pos = line.find('"', pos);
   while (pos != string::npos) {
      cout << pos << ' ';
      pos = line.find('"', pos + 1);
   }
   cout << endl;
}
[[in]]--------------------------------------------------
dijo "puke" y todos constestamos "puke too"
[[out]]-------------------------------------------------
5 10 33 42 
