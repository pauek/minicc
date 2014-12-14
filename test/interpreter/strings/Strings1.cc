#include <iostream>
using namespace std;

int main() {
   string line;
   getline(cin, line);
   int pos1, pos2 = 0;
   pos1 = line.find('"', pos2 + 1);
   pos2 = line.find('"', pos1 + 1);
   while (pos1 != string::npos && pos2 != string::npos) {
      cout << line.substr(pos1 + 1, pos2 - pos1 - 1) << endl;
      pos1 = line.find('"', pos2 + 1);
      pos2 = line.find('"', pos1 + 1);
   }
   cout << endl;
}
[[in]]--------------------------------------------------
a "b" c "d e f" "g" h i j k l m "o p q" r s "t "u v" x y z "
[[out]]-------------------------------------------------
b
d e f
g
o p q
t 
 x y z 

