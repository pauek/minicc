
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
""""""
[[out]]-------------------------------------------------
0 1 2 3 4 5 
