#include <iostream>
using namespace std;

int main() {
   string s;
   while (getline(cin, s)) {
      cout << s.substr(s.size()/2) 
           << s.substr(0, s.size()/2) << endl;
   }
}
[[in]]---------------------------------------------------
cortaCORTA
don'tcutthroughhere
[[out]]--------------------------------------------------
CORTAcorta
hroughheredon'tcutt
