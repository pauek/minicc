#include <iostream>
#include <list>
#include <sstream>
using namespace std;

int main() {
   istringstream S("percha sarten hola agua zarpa tarta");
   list<string> L;
   string n;
   while (S >> n) { 
      L.push_back(n); 
   }
   L.sort();
   list<string>::iterator it;
   for (it = L.begin(); it != L.end(); it++) {
      cout << *it << ' ';
   }
   cout << endl;
}
[[out]]--------------------------------------------------
agua hola percha sarten tarta zarpa 
